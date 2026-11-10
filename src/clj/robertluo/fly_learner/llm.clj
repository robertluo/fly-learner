(ns robertluo.fly-learner.llm
  "Language models: Anthropic's (the official Java SDK) and any other through
  an OpenAI-compatible server (oMLX, LM Studio, llama.cpp, vLLM, Ollama's
  /v1) or Ollama's own API. Configured in config.edn (`settings`):

    :llm {:providers {:anthropic {:type :anthropic :keychain {:service \"learner\" :account \"anthropic\"}}
                      :omlx      {:type :openai :base-url \"http://localhost:8000/v1\" :api-key-env \"OMLX_API_KEY\"}
                      :ollama    {:type :ollama :num-ctx 32768}}
          :default   :anthropic
          :models    {:strong \"claude-sonnet-5-5\" :common \"omlx/Qwen3.8-27B-4bit\"}}

  A model is named \"provider/model\" for one of the providers, else it is
  the default provider's. Two tiers name the models the rest uses:
    :strong  synthesis of ideas: section/chapter/document summaries, quizzes
    :common  high-volume routine work: summarising runs of paragraphs

  A provider: :type, :base-url, its key (:api-key-env, an environment
  variable; :keychain, a macOS keychain item; or :api-key; Anthropic's also
  ANTHROPIC_API_KEY), :timeout-s (600), :concurrency (requests at a time,
  unlimited when absent), :body (fields merged into every request, e.g.
  {:temperature 0.2}); an :openai one :json (:schema, the server's JSON
  schema mode, the default; :prompt, the schema in the prompt only); an
  :ollama one :num-ctx (the context length, 32768), :think.

  Only Anthropic's models get structured outputs that are guaranteed, the
  system prompt cached and a server-side fallback on refusals; elsewhere a
  JSON reply is asked for with its schema (and in the server's JSON mode
  when it has one), read leniently (thinking and code fences dropped) and
  asked for again once when it is no JSON."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [jsonista.core :as json])
  (:import (com.anthropic.client AnthropicClient)
           (com.anthropic.client.okhttp AnthropicOkHttpClient)
           (com.anthropic.core JsonValue)
           (com.anthropic.models.messages CacheControlEphemeral ContentBlock JsonOutputFormat
                                          JsonOutputFormat$Schema Message MessageCreateParams
                                          OutputConfig OutputConfig$Effort StopReason TextBlock
                                          TextBlockParam Usage)
           (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse HttpResponse$BodyHandlers)
           (java.time Duration)
           (java.util.concurrent Semaphore)))

(def default-models {:strong "claude-sonnet-5-5" :common "claude-haiku-5-5"})

(defn model-for
  "Resolve a tier keyword (:strong / :common) or a model id string."
  [models tier-or-id]
  (if (keyword? tier-or-id) (get (merge default-models models) tier-or-id) tier-or-id))

;; --- settings -------------------------------------------------------------

(defn settings
  "The model settings of config.edn `cfg`: {:providers {key provider}
  :default key :models {tier model}}, from its :llm map (see the ns doc),
  else from the older {:anthropic {:keychain .. :models ..}}: Anthropic
  alone. An :anthropic provider is always there."
  [cfg]
  (if-let [{:keys [providers default models]} (:llm cfg)]
    {:providers (merge {:anthropic {:type :anthropic}} providers)
     :default   (or default :anthropic)
     :models    models}
    {:providers {:anthropic (merge {:type :anthropic} (select-keys (:anthropic cfg) [:keychain]))}
     :default   :anthropic
     :models    (get-in cfg [:anthropic :models])}))

^:rct/test
(comment
  (settings {:anthropic {:keychain {:service "s" :account "a"} :models {:common "claude-x"}}})
  ;=> {:providers {:anthropic {:type :anthropic :keychain {:service "s" :account "a"}}} :default :anthropic :models {:common "claude-x"}}
  (:providers (settings {:llm {:providers {:local {:type :openai :base-url "http://h/v1"}} :models {:common "local/q"}}}))
  ;=> {:anthropic {:type :anthropic} :local {:type :openai :base-url "http://h/v1"}}
  )

;; --- cost tracking --------------------------------------------------------

(def prices
  "USD per million tokens [input output] (Anthropic list prices, 2026-10;
  Haiku rate applies to prompts <= 100K tokens). Cache writes bill 1.25x
  input, cache reads 0.1x input. A model not here (a local one) is free."
  {"claude-opus-5-5"   [4.00 20.00]
   "claude-sonnet-5-5" [2.00 10.00]
   "claude-haiku-5-5"  [0.10 0.50]})

(defonce ^{:doc "Running token totals per model for this process."}
  usage (atom {}))

(defn- record-usage!
  "Add a request's tokens ({:input :cache-write :cache-read :output}) to `model`'s."
  [model tokens]
  (swap! usage update model #(merge-with + (or % {}) {:requests 1} tokens)))

(defn cost
  "Approximate USD spent per model (and :total) from `usage` totals."
  ([] (cost @usage))
  ([totals]
   (let [per (into {}
                   (for [[model {:keys [input cache-write cache-read output]}] totals
                         :let [[in out] (prices model [0 0])]]
                     [model (/ (+ (* in (+ (or input 0) (* 1.25 (or cache-write 0)) (* 0.1 (or cache-read 0))))
                                  (* out (or output 0)))
                               1e6)]))]
     (assoc per :total (reduce + (vals per))))))

;; --- client ---------------------------------------------------------------

(defn- keychain-password
  "Read a generic password from the macOS keychain; nil if not found, or
  where there is no keychain (Linux, a container)."
  [{:keys [service account]}]
  (try (let [{:keys [exit out]} (sh/sh "security" "find-generic-password"
                                       "-s" service "-a" account "-w")]
         (when (zero? exit) (str/trim out)))
       (catch java.io.IOException _ nil)))

(defn key-source
  "Where provider `p`'s key comes from, first found: {:from :saved (its
  :api-key, typed in the app) | :env (its :api-key-env, or Anthropic's
  ANTHROPIC_API_KEY) | :keychain, :key .., :name (the variable or item)};
  nil when it has none (a local server may need none)."
  [{:keys [type api-key-env keychain] :as p}]
  (or (when-let [k (not-empty (:api-key p))] {:from :saved :key k})
      (when-let [k (some-> api-key-env System/getenv not-empty)] {:from :env :key k :name api-key-env})
      (when-let [k (and (= :anthropic type) (not-empty (System/getenv "ANTHROPIC_API_KEY")))]
        {:from :env :key k :name "ANTHROPIC_API_KEY"})
      (when-let [k (some-> keychain keychain-password not-empty)]
        {:from :keychain :key k :name (str (:service keychain) "/" (:account keychain))})))

(defn- api-key [p] (:key (key-source p)))

(defn- connect
  "What provider `p` sends its requests with."
  [{:keys [type base-url timeout-s] :or {timeout-s 600} :as p}]
  (case type
    :anthropic (let [b (AnthropicOkHttpClient/builder)]
                 (.fromEnv b)
                 (when-let [k (api-key p)] (.apiKey b ^String k))
                 (when base-url (.baseUrl b ^String base-url))
                 (.build b))
    (:openai :ollama) {:http    (-> (HttpClient/newBuilder)
                                    (.version java.net.http.HttpClient$Version/HTTP_1_1)
                                    (.connectTimeout (Duration/ofSeconds 10))
                                    .build)
                       :key     (api-key p)
                       :timeout (Duration/ofSeconds timeout-s)}))

(defn client
  "A client for the providers of `settings` (see `settings`), each connected
  on its first request (a configured provider needs no key until used)."
  ([] (client (settings {})))
  ([{:keys [providers default]}]
   {:default   (or default :anthropic)
    :providers (into {} (for [[k p] providers]
                          [k (assoc p
                                    :key  k
                                    :conn (delay (connect p))
                                    ;; its server refused a JSON schema: the schema goes in the prompt only
                                    :schema-refused (atom false)
                                    :gate (some-> (:concurrency p) int (Semaphore. true)))]))}))

(defn- route
  "[provider model-id] for model `model`: \"provider/id\" names one of
  `client`'s providers; anything else is the default provider's."
  [{:keys [providers default]} model]
  (let [[_ p id] (re-matches #"([^/]+)/(.+)" (str model))
        k        (some-> p keyword)]
    (if (contains? providers k)
      [(providers k) id]
      [(or (providers default) (throw (ex-info (str "No provider " default " configured") {:model model})))
       model])))

^:rct/test
(comment
  (let [c (client {:providers {:anthropic {:type :anthropic} :omlx {:type :openai}} :default :anthropic})]
    [(mapv (comp :key first) [(route c "omlx/mlx-community/Qwen") (route c "claude-sonnet-5-5") (route c "hf.co/x/y")])
     (second (route c "omlx/mlx-community/Qwen"))])
  ;=> [[:omlx :anthropic :anthropic] "mlx-community/Qwen"]
  )

;; --- Anthropic ------------------------------------------------------------

(def ^:private server-fallback-models
  "Models that accept the server-side refusal fallback (`fallbacks: \"default\"`).
  Haiku has none; callers retry on a stronger model instead."
  #{"claude-opus-5-5" "claude-sonnet-5-5"})

(defn- text-of [^Message msg]
  (->> (.content msg)
       (keep (fn [^ContentBlock b] (some-> ^TextBlock (.orElse (.text b) nil) .text)))
       (apply str)))

(defn- output-format
  "Structured-output format from a JSON-schema map (keyword keys allowed)."
  ^JsonOutputFormat [schema]
  (let [sb (JsonOutputFormat$Schema/builder)]
    (doseq [[k v] (walk/stringify-keys schema)]
      (.putAdditionalProperty sb ^String k (JsonValue/from v)))
    (-> (JsonOutputFormat/builder) (.schema (.build sb)) (.build))))

(defn- cached-system
  "System prompt as one text block with a cache breakpoint at its end.
  Only caches when the prefix reaches the model's minimum (512 tokens on
  current models); shorter prompts are silently uncached."
  [^String text]
  [(-> (TextBlockParam/builder)
       (.text text)
       (.cacheControl (.build (CacheControlEphemeral/builder)))
       (.build))])

(defn- anthropic-complete
  "One request to Anthropic -> [text tokens]."
  [^AnthropicClient client {:keys [prompt system model max-tokens effort schema]}]
  (let [oc   (cond-> (.effort (OutputConfig/builder) (OutputConfig$Effort/of (name effort)))
               schema (.format (output-format schema)))
        b    (doto (MessageCreateParams/builder)
               (.model ^String model)
               (.maxTokens (long max-tokens))
               (.outputConfig (.build oc))
               (.addUserMessage ^String prompt))
        _    (when system (.systemOfTextBlockParams b (cached-system system)))
        _    (when (server-fallback-models model)
               ;; on a safety-classifier refusal, the server retries on a fallback model
               (.putAdditionalHeader b "anthropic-beta" "server-side-fallback-2026-07-01")
               (.putAdditionalBodyProperty b "fallbacks" (JsonValue/from "default")))
        msg  (.create (.messages client) (.build b))
        stop (.orElse (.stopReason msg) nil)
        ^Usage u (.usage msg)]
    [(cond
       (= StopReason/REFUSAL stop)    {:stop-reason :refusal}
       (= StopReason/MAX_TOKENS stop) {:stop-reason :max-tokens}
       :else                          (text-of msg))
     {:input       (.inputTokens u)
      :cache-write (.orElse (.cacheCreationInputTokens u) 0)
      :cache-read  (.orElse (.cacheReadInputTokens u) 0)
      :output      (.outputTokens u)}]))

;; --- OpenAI-compatible servers and Ollama ---------------------------------

(declare http-problem)

(defn- post!
  "POST `body` as JSON to `url` with connection `conn`; the reply's JSON
  (string keys). Throws on an HTTP error, with its :status and :body."
  [{:keys [^HttpClient http key ^Duration timeout]} url body]
  (let [req  (cond-> (HttpRequest/newBuilder (URI. url))
               true (.timeout timeout)
               true (.header "Content-Type" "application/json")
               key  (.header "Authorization" (str "Bearer " key))
               true (.POST (HttpRequest$BodyPublishers/ofString (json/write-value-as-string body))))
        ^HttpResponse resp (.send http (.build req) (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (if (<= 200 status 299)
      (json/read-value ^String (.body resp))
      (throw (ex-info (http-problem status url (.body resp)) {:status status :body (.body resp) :url url})))))

(defn- http-problem
  "What HTTP `status` from `url` with `body` says, in words."
  [status url body]
  (let [b (str body)]
    (str (case (int status)
           (401 403) "The server wants an API key, or another one"
           404       "Nothing there: is the address right?"
           (str "HTTP " status))
         " (" url (when-not (#{401 403} status) (str ": " (subs b 0 (min 160 (count b))))) ")")))

(defn- get!
  "GET `url` with connection `conn`; the reply's JSON (string keys)."
  [{:keys [^HttpClient http key]} url]
  (let [req  (cond-> (HttpRequest/newBuilder (URI. url))
               true (.timeout (Duration/ofSeconds 15))
               key  (.header "Authorization" (str "Bearer " key))
               true .GET)
        ^HttpResponse resp (.send http (.build req) (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (if (<= 200 status 299)
      (json/read-value ^String (.body resp))
      (throw (ex-info (http-problem status url (.body resp)) {:status status :url url})))))

(declare base)

(defn list-models
  "The ids of the models provider `p` (a provider map, see the ns doc) serves,
  asked of it: what a tier may name after \"provider/\"."
  [{:keys [type base-url] :as p}]
  (let [conn (connect p)]
    (case type
      :anthropic (mapv #(.id ^com.anthropic.models.models.ModelInfo %) (.data (.list (.models ^AnthropicClient conn))))
      :openai    (mapv #(get % "id") (get (get! conn (str (base base-url "http://localhost:8000/v1") "/models")) "data"))
      :ollama    (mapv #(get % "name") (get (get! conn (str (base base-url "http://localhost:11434") "/api/tags")) "models")))))

(defn- messages [system prompt]
  (cond-> [] system (conj {:role "system" :content system}) true (conj {:role "user" :content prompt})))

(defn- with-schema
  "`prompt` asking for a reply matching `schema`."
  [prompt schema]
  (str prompt "\n\nReply with one JSON object and nothing else, matching this JSON Schema:\n"
       (json/write-value-as-string schema)))

(defn- base [url default] (str/replace (or url default) #"/+$" ""))

(defn- openai-complete
  "One request to an OpenAI-compatible server -> [text tokens]."
  [{:keys [base-url json body conn schema-refused]} {:keys [prompt system model max-tokens schema]}]
  (let [url      (str (base base-url "http://localhost:8000/v1") "/chat/completions")
        schema?  (and schema (not= :prompt json) (not @schema-refused))
        request  (merge {:model      model
                         :messages   (messages system (cond-> prompt schema (with-schema schema)))
                         :max_tokens max-tokens
                         :stream     false}
                        (when schema?
                          {:response_format {:type "json_schema"
                                             :json_schema {:name "reply" :schema schema :strict true}}})
                        body)
        reply    (try (post! @conn url request)
                      (catch clojure.lang.ExceptionInfo e
                        (if (and schema? (#{400 415 422 500 501} (:status (ex-data e))))
                          ;; no JSON schema mode there: ask in the prompt alone from now on
                          (do (reset! schema-refused true)
                              (post! @conn url (dissoc request :response_format)))
                          (throw e))))
        choice   (get-in reply ["choices" 0])
        u        (get reply "usage")
        cached   (or (get-in u ["prompt_tokens_details" "cached_tokens"]) 0)]
    [(case (get choice "finish_reason")
       "length"         {:stop-reason :max-tokens}
       "content_filter" {:stop-reason :refusal}
       (str (get-in choice ["message" "content"])))
     {:input (- (or (get u "prompt_tokens") 0) cached) :cache-read cached :output (or (get u "completion_tokens") 0)}]))

(defn- ollama-complete
  "One request to Ollama's own API -> [text tokens]."
  [{:keys [base-url num-ctx think body conn] :or {num-ctx 32768}} {:keys [prompt system model max-tokens schema]}]
  (let [request (merge {:model    model
                        :messages (messages system (cond-> prompt schema (with-schema schema)))
                        :stream   false
                        :options  {:num_ctx num-ctx :num_predict max-tokens}}
                       (when schema {:format schema})
                       (when (some? think) {:think think})
                       body)
        reply   (post! @conn (str (base base-url "http://localhost:11434") "/api/chat") request)]
    [(if (= "length" (get reply "done_reason"))
       {:stop-reason :max-tokens}
       (str (get-in reply ["message" "content"])))
     {:input (or (get reply "prompt_eval_count") 0) :output (or (get reply "eval_count") 0)}]))

(defn- without-thinking
  "A local model's reply without the reasoning it wrote before it."
  [s]
  (str/trim (str/replace s #"(?s)^\s*<think>.*?</think>" "")))

;; --- requests -------------------------------------------------------------

(defn complete
  "Single request -> response text.
  opts: :prompt (required)
        :system  string; sent with a cache breakpoint, so keep it identical
                 across calls and put per-call content in :prompt
        :model   model (default: the :strong tier's default), \"provider/id\"
                 or an id of the default provider (see `route`)
        :max-tokens :effort (:low .. :max; Anthropic's models)
        :schema  JSON schema map; Anthropic's reply is then guaranteed to
                 match it, other models' are asked to (`complete-json`)."
  [client {:keys [model] :or {model (:strong default-models)} :as opts}]
  (let [[{:keys [type ^Semaphore gate conn] :as p} id] (route client model)
        opts (merge {:max-tokens 16000 :effort :medium} opts {:model id})]
    (when gate (.acquire gate))
    (try
      (let [[text tokens] (case type
                            :anthropic (anthropic-complete @conn opts)
                            :openai    (openai-complete p opts)
                            :ollama    (ollama-complete p opts))]
        (record-usage! model tokens)
        (if (map? text)
          (throw (ex-info (if (= :refusal (:stop-reason text)) "Request refused" "Reply truncated at max-tokens")
                          (assoc text :model model)))
          (cond-> text (not= :anthropic type) without-thinking)))
      (finally (when gate (.release gate))))))

(defn refusal? [e] (= :refusal (:stop-reason (ex-data e))))

(defn json-reply
  "The JSON object in a model's reply `s` (keyword keys): code fences and
  text around it ignored."
  [s]
  (let [t (str/trim (str/replace (str s) #"(?s)^```(?:json)?\s*(.*?)\s*```$" "$1"))
        t (if (str/starts-with? t "{")
            t
            (let [i (str/index-of t "{"), j (str/last-index-of t "}")]
              (if (and i j (< i j)) (subs t i (inc j)) t)))]
    (json/read-value t json/keyword-keys-object-mapper)))

^:rct/test
(comment
  (json-reply "```json\n{\"a\": [1, 2]}\n```") ;=> {:a [1 2]}
  (json-reply "Here it is: {\"b\": \"x\"} Hope it helps.") ;=> {:b "x"}
  )

(defn complete-json
  "Like `complete` with a required :schema; returns the parsed reply
  (keyword keys). A model that guarantees nothing is asked again once when
  its reply is no JSON."
  [client opts]
  {:pre [(:schema opts)]}
  (let [[{:keys [type]}] (route client (or (:model opts) (:strong default-models)))]
    (if (= :anthropic type)
      (json/read-value (complete client opts) json/keyword-keys-object-mapper)
      (try (json-reply (complete client opts))
           (catch com.fasterxml.jackson.core.JacksonException _
             (json-reply (complete client (update opts :prompt str
                                                  "\n\nYour previous reply was not valid JSON. Reply with the JSON object only."))))))))
