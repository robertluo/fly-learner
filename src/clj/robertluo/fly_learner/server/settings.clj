(ns robertluo.fly-learner.server.settings
  "The learner's settings, edited in the app (app layer): which models do
  the work and who serves them, so nothing needs editing by hand.

  Saved in data/settings.edn ({:llm {:providers :default :models}}, over
  config.edn's :llm, robertluo.fly-learner.llm/settings, which is only the
  first run's), and the API keys typed in the app in data/secrets.edn
  ({provider-name key}, readable by its owner only), never sent back to
  the page. In force at once: `llm` reads them, `client` is a model client
  for them, built again when they change."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [robertluo.fly-learner.llm :as llm])
  (:import (java.nio.file Files)
           (java.nio.file.attribute PosixFilePermissions)))

(def settings-file "data/settings.edn")
(def secrets-file "data/secrets.edn")

(defn- read-edn [path]
  (let [f (io/file path)]
    (if (.isFile f)
      (let [m (edn/read-string (slurp f))] (if (map? m) m {}))
      {})))

(defn- write-edn!
  "Write `m` to `path`; `private?`: readable and writable by its owner only."
  [path m private?]
  (let [f (io/file path)]
    (io/make-parents f)
    (when (and private? (not (.exists f)))
      (.createNewFile f))
    (when private?
      (try (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rw-------"))
           (catch UnsupportedOperationException _ nil)))
    (spit f (with-out-str (prn m)))))

(defn llm
  "The model settings in force (llm/settings shape): config.edn's, replaced
  by what was saved in the app; keys typed in the app given to their
  providers."
  [cfg]
  (let [base    (llm/settings cfg)
        saved   (:llm (read-edn settings-file))
        secrets (read-edn secrets-file)
        s       (merge base (select-keys saved [:providers :default :models]))]
    (update s :providers
            (fn [ps] (into {} (for [[k p] ps]
                                [k (cond-> p (get secrets (name k)) (assoc :api-key (get secrets (name k))))]))))))

(defonce ^:private built (atom [nil nil])) ; [settings client]

(defn client
  "A model client for the settings in force (llm/client), the same until
  they change."
  [cfg]
  (let [s (llm cfg)
        [was c] @built]
    (if (= s was)
      c
      (let [c (llm/client s)]
        (reset! built [s c])
        c))))

(defn client-ref
  "What derefs to `client` now: for those holding a client for long."
  [cfg]
  (reify clojure.lang.IDeref (deref [_] (client cfg))))

;;=============================================================================
;; The page's view of them, and its edits
;;=============================================================================

(def ^:private editable [:type :base-url :api-key-env :concurrency :num-ctx :timeout-s])

(defn view
  "The settings for the page: {:providers [{:name :type :base-url .. :key
  {:from :hint :name}}] :default :models :saved?}; no key is in it, only
  where it comes from and its last four characters."
  [cfg]
  (let [{:keys [providers default models]} (llm cfg)]
    {:providers (vec (for [[k p] (sort-by key providers)]
                       (cond-> (assoc (select-keys p editable) :name (name k))
                         (llm/key-source p)
                         (assoc :key (let [{:keys [from key name]} (llm/key-source p)]
                                       (cond-> {:from from :hint (str "…" (subs key (max 0 (- (count key) 4))))}
                                         name (assoc :name name)))))))
     :default   (some-> default name)
     :models    (merge llm/default-models models)
     :saved?    (boolean (:llm (read-edn settings-file)))}))

(defn- problem
  "What is wrong with `edit` ({:providers [..] :default :models}), or nil."
  [{:keys [providers default models]}]
  (let [names (map :name providers)]
    (cond
      (not (and (sequential? providers) (seq providers))) "Keep at least one provider"
      (not (every? #(and (string? %) (re-matches #"[a-z][a-z0-9_-]{0,31}" %)) names))
      "A provider's name is lowercase letters, digits, - or _"
      (not (apply distinct? names)) "Two providers have the same name"
      (not (every? #(#{:anthropic :openai :ollama} (:type %)) providers)) "A provider is Anthropic, OpenAI-compatible or Ollama"
      (not (every? #(or (str/blank? (:base-url %)) (re-matches #"https?://\S+" (:base-url %))) providers))
      "A server's address starts with http:// or https://"
      (not (every? (fn [p] (every? #(or (nil? (get p %)) (pos-int? (get p %))) [:concurrency :num-ctx :timeout-s])) providers))
      "Numbers are whole and above zero"
      (not (some #{default} names)) "The default provider must be one of them"
      (not (every? #(and (string? (get models %)) (not (str/blank? (get models %)))) [:strong :common]))
      "Both tiers need a model")))

(defn save!
  "Save the page's `edit`: {:providers [{:name :type :base-url :api-key-env
  :concurrency :num-ctx :timeout-s :api-key (typed: replaces the saved key)
  :forget-key? (drops it)}] :default name :models {:strong :common}}.
  Settings not shown on the page (a keychain item, :body, ..) are kept.
  Returns {:error msg} or the new `view`."
  [cfg edit]
  (if-let [msg (problem edit)]
    {:error msg}
    (let [old     (:providers (llm cfg))
          secrets (read-edn secrets-file)
          clean   #(into {} (remove (fn [[_ v]] (or (nil? v) (and (string? v) (str/blank? v))))) %)
          provs   (into {} (for [{:keys [name] :as p} (:providers edit)
                                 :let [k (keyword name)]]
                             [k (merge (apply dissoc (dissoc (get old k) :api-key) editable)
                                       (clean (select-keys p editable)))]))
          secrets (reduce (fn [m {:keys [name api-key forget-key?]}]
                            (cond forget-key?                (dissoc m name)
                                  (not (str/blank? api-key)) (assoc m name (str/trim api-key))
                                  :else                      m))
                          (select-keys secrets (map :name (:providers edit)))
                          (:providers edit))]
      (write-edn! settings-file
                  (assoc (read-edn settings-file)
                         :llm {:providers provs :default (keyword (:default edit)) :models (:models edit)})
                  false)
      (write-edn! secrets-file secrets true)
      (view cfg))))

(defn provider-for
  "Provider `p` as the page has it (maybe not saved yet), its key the one
  typed, else the one saved for its name: to ask what models it serves."
  [cfg {:keys [name api-key] :as p}]
  (let [saved (get-in (llm cfg) [:providers (keyword name)])]
    (cond-> (merge (dissoc saved :api-key) (select-keys p editable))
      (not (str/blank? api-key))       (assoc :api-key (str/trim api-key))
      (and (str/blank? api-key) saved) (merge (select-keys saved [:api-key :keychain])))))

^:rct/test
(comment
  (problem {:providers [{:name "anthropic" :type :anthropic}] :default "anthropic"
            :models {:strong "claude-sonnet-5-5" :common "claude-haiku-5-5"}}) ;=> nil
  (problem {:providers [{:name "Bad Name" :type :openai}] :default "Bad Name" :models {}})
  ;=> "A provider's name is lowercase letters, digits, - or _"
  (problem {:providers [{:name "a" :type :openai :base-url "localhost:8000"}] :default "a" :models {}})
  ;=> "A server's address starts with http:// or https://"
  (problem {:providers [{:name "a" :type :openai}] :default "b" :models {}}) ;=> "The default provider must be one of them"
  )
