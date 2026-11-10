(ns robertluo.fly-learner.prompts
  "The instructions given to the models, editable by the learner.

  Each namespace that calls a model registers its system prompts here
  (`prompt!`, at load: an id, where it is used, its default text) and reads
  them when it calls (`text`): the learner's version when they wrote one,
  else the default. Edits are kept in a file (`use-file!`, the app's
  data/prompts.edn), so they survive restarts and a newer default does not
  overwrite them; `reset!` goes back to the default.

  Depends on nothing: any layer may use it."
  (:refer-clojure :exclude [reset!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defonce ^:private registry (atom {}))  ; id -> {:id :group :title :about :default :order}
(defonce ^:private edits (atom {}))     ; id -> the learner's text
(defonce ^:private file (atom nil))     ; where edits are kept, nil: nowhere

(defn prompt!
  "Register prompt `id`: {:group \"Reading\" :title .. :about (when it is
  used, by which model tier, what the code expects back) :default text}.
  Returns `id`, to pass to `text`."
  [id {:keys [default] :as m}]
  {:pre [(keyword? id) (string? default)]}
  (swap! registry (fn [r] (assoc r id (assoc m :id id :order (get-in r [id :order] (count r))))))
  id)

(defn text
  "The text of prompt `id`: the learner's version, else its default."
  [id]
  (or (get @edits id)
      (:default (get @registry id))
      (throw (ex-info (str "No prompt " id) {:id id}))))

(defn- save! []
  (when-let [f @file]
    (io/make-parents f)
    (spit f (with-out-str (prn (into (sorted-map) @edits))))))

(defn use-file!
  "Keep the learner's edits in file `path` (EDN {id text}), reading those
  there now."
  [path]
  (clojure.core/reset! file (io/file path))
  (clojure.core/reset! edits (let [f (io/file path)]
                               (if (.isFile f)
                                 (let [m (edn/read-string (slurp f))] (if (map? m) m {}))
                                 {})))
  path)

(defn edit!
  "The learner's version of prompt `id`: `s` (blank: the default again)."
  [id s]
  (when-not (contains? @registry id) (throw (ex-info (str "No prompt " id) {:id id})))
  (if (or (nil? s) (re-matches #"\s*" s))
    (swap! edits dissoc id)
    (swap! edits assoc id s))
  (save!)
  id)

(defn reset!
  "Prompt `id` back to its default."
  [id]
  (edit! id nil))

(defn all
  "Every prompt, in the order registered: {:id :group :title :about :default
  :text :edited?}."
  []
  (->> (vals @registry)
       (sort-by :order)
       (mapv (fn [{:keys [id] :as p}]
               (assoc (dissoc p :order) :text (text id) :edited? (contains? @edits id))))))

^:rct/test
(comment
  (prompt! ::example {:group "Test" :title "Example" :about "Not used." :default "Do this."}) ;=> ::example
  (text ::example) ;=> "Do this."
  (do (edit! ::example "Do that.") (text ::example)) ;=> "Do that."
  (:edited? (some #(when (= ::example (:id %)) %) (all))) ;=> true
  (do (reset! ::example) (text ::example)) ;=> "Do this."
  (do (swap! registry dissoc ::example) nil) ;=> nil
  )
