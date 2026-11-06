(ns learner.llm
  "Thin wrapper over the official Anthropic Java SDK.
  API key: ANTHROPIC_API_KEY if set, else the macOS keychain item named in
  config.edn under [:anthropic :keychain]."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import (com.anthropic.client AnthropicClient)
           (com.anthropic.client.okhttp AnthropicOkHttpClient)
           (com.anthropic.core JsonValue)
           (com.anthropic.models.messages ContentBlock Message MessageCreateParams
                                          OutputConfig OutputConfig$Effort StopReason TextBlock)))

(def default-model "claude-opus-5-5")

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

(defn- text-of [^Message msg]
  (->> (.content msg)
       (keep (fn [^ContentBlock b] (some-> ^TextBlock (.orElse (.text b) nil) .text)))
       (apply str)))

(defn complete
  "Single request -> response text.
  opts: :prompt (required) :system :model :max-tokens :effort (:low .. :max)."
  [^AnthropicClient client {:keys [prompt system model max-tokens effort]
                            :or   {model default-model max-tokens 16000 effort :high}}]
  (let [b (doto (MessageCreateParams/builder)
            (.model ^String model)
            (.maxTokens (long max-tokens))
            (.outputConfig (-> (OutputConfig/builder)
                               (.effort (OutputConfig$Effort/of (name effort)))
                               (.build)))
            ;; on a safety-classifier refusal, let the server retry on a fallback model
            (.putAdditionalHeader "anthropic-beta" "server-side-fallback-2026-07-01")
            (.putAdditionalBodyProperty "fallbacks" (JsonValue/from "default"))
            (.addUserMessage ^String prompt))
        _   (when system (.system b ^String system))
        msg (.create (.messages client) (.build b))]
    (when (= StopReason/REFUSAL (.orElse (.stopReason msg) nil))
      (throw (ex-info "Request refused" {:stop-reason :refusal})))
    (text-of msg)))
