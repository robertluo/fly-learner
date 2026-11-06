(ns learner.llm
  "Thin wrapper over the official Anthropic Java SDK.
  API key: ANTHROPIC_API_KEY if set, else the macOS keychain item named in
  config.edn under [:anthropic :keychain].

  Two model tiers (config.edn [:anthropic :models]):
    :strong  synthesis of ideas: section/chapter/document summaries, quizzes
    :common  high-volume routine work: summarising runs of paragraphs"
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
                                          TextBlockParam Usage)))

(def default-models {:strong "claude-sonnet-5-5" :common "claude-haiku-5-5"})

(defn model-for
  "Resolve a tier keyword (:strong / :common) or a model id string."
  [models tier-or-id]
  (if (keyword? tier-or-id) (get (merge default-models models) tier-or-id) tier-or-id))

;; --- cost tracking --------------------------------------------------------

(def prices
  "USD per million tokens [input output] (Anthropic list prices, 2026-10;
  Haiku rate applies to prompts <= 100K tokens). Cache writes bill 1.25x
  input, cache reads 0.1x input."
  {"claude-opus-5-5"   [4.00 20.00]
   "claude-sonnet-5-5" [2.00 10.00]
   "claude-haiku-5-5"  [0.10 0.50]})

(defonce ^{:doc "Running token totals per model for this process."}
  usage (atom {}))

(defn- record-usage! [model ^Usage u]
  (swap! usage update model
         (fn [m]
           (merge-with + (or m {})
                       {:requests    1
                        :input       (.inputTokens u)
                        :cache-write (.orElse (.cacheCreationInputTokens u) 0)
                        :cache-read  (.orElse (.cacheReadInputTokens u) 0)
                        :output      (.outputTokens u)}))))

(defn cost
  "Approximate USD spent per model (and :total) from `usage` totals."
  ([] (cost @usage))
  ([totals]
   (let [per (into {}
                   (for [[model {:keys [input cache-write cache-read output]}] totals
                         :let [[in out] (prices model [0 0])]]
                     [model (/ (+ (* in (+ input (* 1.25 cache-write) (* 0.1 cache-read)))
                                  (* out output))
                               1e6)]))]
     (assoc per :total (reduce + (vals per))))))

;; --- client ---------------------------------------------------------------

(defn- keychain-password
  "Read a generic password from the macOS keychain; nil if not found."
  [{:keys [service account]}]
  (let [{:keys [exit out]} (sh/sh "security" "find-generic-password"
                                  "-s" service "-a" account "-w")]
    (when (zero? exit) (str/trim out))))

(defn client
  "Build a client. `opts` is the :anthropic map from config.edn."
  (^AnthropicClient [] (client {}))
  (^AnthropicClient [{:keys [keychain]}]
   (let [b (AnthropicOkHttpClient/builder)]
     (.fromEnv b)
     (when-not (System/getenv "ANTHROPIC_API_KEY")
       (when-let [k (some-> keychain keychain-password)]
         (.apiKey b ^String k)))
     (.build b))))

;; --- requests -------------------------------------------------------------

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

(defn complete
  "Single request -> response text.
  opts: :prompt (required)
        :system  string; sent with a cache breakpoint, so keep it identical
                 across calls and put per-call content in :prompt
        :model   model id (default: the :strong tier)
        :max-tokens :effort (:low .. :max)
        :schema  JSON schema map; the reply is then guaranteed to match it."
  [^AnthropicClient client {:keys [prompt system model max-tokens effort schema]
                            :or   {model (:strong default-models) max-tokens 16000 effort :medium}}]
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
        stop (.orElse (.stopReason msg) nil)]
    (record-usage! model (.usage msg))
    (cond
      (= StopReason/REFUSAL stop)    (throw (ex-info "Request refused" {:stop-reason :refusal :model model}))
      (= StopReason/MAX_TOKENS stop) (throw (ex-info "Reply truncated at max-tokens" {:stop-reason :max-tokens :model model}))
      :else (text-of msg))))

(defn refusal? [e] (= :refusal (:stop-reason (ex-data e))))

(defn complete-json
  "Like `complete` with a required :schema; returns the parsed reply (keyword keys)."
  [client opts]
  {:pre [(:schema opts)]}
  (json/read-value (complete client opts) json/keyword-keys-object-mapper))
