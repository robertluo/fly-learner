(ns robertluo.fly-learner.llm-test
  "Models other than Anthropic's, against a stand-in server."
  (:require [clojure.test :refer [deftest is testing]]
            [jsonista.core :as json]
            [org.httpkit.server :as http-kit]
            [robertluo.fly-learner.llm :as llm]))

(defn- serve!
  "A model server answering with (reply request-json uri); its requests in
  the returned atom. Returns [base-url requests stop]."
  [reply]
  (let [seen (atom [])
        s    (http-kit/run-server
              (fn [{:keys [uri body headers]}]
                (let [req (json/read-value (slurp body))]
                  (swap! seen conj {:uri uri :body req :auth (get headers "authorization")})
                  (let [[status r] (reply req uri)]
                    {:status status :headers {"Content-Type" "application/json"} :body (json/write-value-as-string r)})))
              {:port 0 :legacy-return-value? false})]
    [(str "http://localhost:" (http-kit/server-port s)) seen #(http-kit/server-stop! s)]))

(def ^:private schema
  {:type "object" :properties {:title {:type "string"}} :required ["title"] :additionalProperties false})

(defn- chat [content & {:keys [finish] :or {finish "stop"}}]
  [200 {:choices [{:message {:content content} :finish_reason finish}]
        :usage   {:prompt_tokens 120 :completion_tokens 30 :prompt_tokens_details {:cached_tokens 100}}}])

(deftest an-openai-compatible-server
  (let [[url seen stop] (serve! (fn [req _]
                                  (if (get req "response_format")
                                    [400 {:error {:message "response_format not supported"}}]
                                    (chat "<think>Let me see.</think>\n```json\n{\"title\": \"Folds\"}\n```"))))
        c (llm/client {:providers {:anthropic {:type :anthropic}
                                   :local     {:type :openai :base-url (str url "/v1/") :api-key "k" :concurrency 2
                                               :body {:temperature 0.2}}}
                       :default :anthropic})]
    (try
      (reset! llm/usage {})
      (is (= {:title "Folds"} (llm/complete-json c {:model "local/mlx-community/Q" :system "Sys" :prompt "Summarise"
                                                    :schema schema})))
      (testing "the model by its id there, the key, the server's JSON schema mode, then the prompt alone"
        (let [[first-try retry] @seen]
          (is (= "/v1/chat/completions" (:uri first-try)))
          (is (= "Bearer k" (:auth first-try)))
          (is (= "mlx-community/Q" (get-in first-try [:body "model"])))
          (is (= 0.2 (get-in first-try [:body "temperature"])))
          (is (= ["system" "user"] (map #(get % "role") (get-in first-try [:body "messages"]))))
          (is (re-find #"JSON Schema" (get-in first-try [:body "messages" 1 "content"])))
          (is (= "json_schema" (get-in first-try [:body "response_format" "type"])))
          (is (nil? (get-in retry [:body "response_format"])))))
      (testing "the server's refusal of a schema is remembered"
        (llm/complete-json c {:model "local/Q" :prompt "Again" :schema schema})
        (is (= 3 (count @seen))))
      (testing "tokens counted under the model's full name, free"
        (is (= {:requests 1 :input 20 :cache-read 100 :output 30} (@llm/usage "local/mlx-community/Q")))
        (is (= 1 (:requests (@llm/usage "local/Q"))))
        (is (zero? (:total (llm/cost {"local/Q" (@llm/usage "local/Q")})))))
      (finally (stop)))))

(deftest replies-that-go-wrong
  (let [n (atom 0)
        [url _ stop] (serve! (fn [_ _]
                               (case (swap! n inc)
                                 1 (chat "Sorry, here: title is Folds")
                                 2 (chat "{\"title\": \"Folds\"}")
                                 3 (chat "{\"title\": \"Fol" :finish "length"))))
        c (llm/client {:providers {:local {:type :openai :base-url url :json :prompt}} :default :local})]
    (try
      (is (= {:title "Folds"} (llm/complete-json c {:model "Q" :prompt "P" :schema schema}))
          "no JSON: asked again once")
      (let [e (try (llm/complete c {:model "Q" :prompt "P"}) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (= :max-tokens (:stop-reason (ex-data e)))))
      (finally (stop)))))

(deftest ollama-s-own-api
  (let [[url seen stop] (serve! (fn [_ _] [200 {:message {:content "{\"title\": \"Folds\"}"} :done_reason "stop"
                                                :prompt_eval_count 50 :eval_count 9}]))
        c (llm/client {:providers {:ollama {:type :ollama :base-url url :num-ctx 8192 :think false}}
                       :default :anthropic})]
    (try
      (is (= {:title "Folds"} (llm/complete-json c {:model "ollama/qwen3:14b" :prompt "P" :schema schema :max-tokens 500})))
      (let [{:keys [uri body]} (first @seen)]
        (is (= "/api/chat" uri))
        (is (= "qwen3:14b" (get body "model")))
        (is (= {"num_ctx" 8192 "num_predict" 500} (get body "options")))
        (is (= "object" (get-in body ["format" "type"])))
        (is (false? (get body "think"))))
      (finally (stop)))))
