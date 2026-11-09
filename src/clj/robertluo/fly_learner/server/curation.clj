(ns robertluo.fly-learner.server.curation
  "The learner correcting the knowledge base, for the API: a concept the
  model missed, a topic or a concept that is no knowledge. App layer: the
  concept edit (robertluo.fly-learner.concept, logged), then what follows
  from it, as an ingest would: topics, prerequisites, goals."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.concept.cluster :as cluster]
            [robertluo.fly-learner.concept.prereq :as prereq]
            [robertluo.fly-learner.learning.store :as store]
            [robertluo.fly-learner.server.study :as study]))

(def ^:private gloss-passages "Passages the model reads to write a gloss." 6)

(defn- clean-names [label aliases]
  (let [label (some-> label str/trim)]
    [label (vec (distinct (remove #(or (str/blank? %) (= (str/lower-case %) (str/lower-case (str label))))
                                  (map str/trim aliases))))]))

(defn add-concept!
  "Add a concept the model missed: `label`, optional `gloss` and `aliases`
  (other names). The passages naming it (concept/naming-nodes) mention it;
  without a gloss the model writes one from them. Then what it needs first
  is judged (concept.prereq) and it joins a topic (cluster/refresh!, goals
  on a regrouped topic follow). Returns {:concept/id :mentions n}, or
  {:error ..} (a name another concept has)."
  [conn client {:keys [models] :as opts} {:concept/keys [label gloss aliases]}]
  (let [[label aliases] (clean-names label aliases)]
    (cond
      (or (str/blank? label) (> (count label) 80))
      {:error {:type :invalid :message "Name the concept (up to 80 characters)"}}

      (or (> (count aliases) 10) (some #(> (count %) 80) aliases) (and gloss (> (count gloss) 500)))
      {:error {:type :invalid :message "Up to 10 other names, and a gloss of up to 500 characters"}}

      (concept/taken (d/db conn) (cons label aliases))
      (let [{:keys [alias] :as c} (concept/taken (d/db conn) (cons label aliases))]
        {:error {:type :invalid :message (str "\"" alias "\" already names the concept " (:concept/label c))
                 :concept/id (:concept/id c)}})

      :else
      (let [db    (d/db conn)
            nodes (concept/naming-nodes db (cons label aliases))
            gloss (if (str/blank? gloss)
                    (when (seq nodes)
                      ((concept/llm-gloss @client opts) label
                       (for [nid (take gloss-passages nodes)
                             :let [{:node/keys [text title]} (d/pull db [:node/text :node/title] [:node/id nid])]]
                         (or text title))))
                    gloss)
            id    (concept/add! conn {:concept/label label :concept/gloss gloss :concept/aliases aliases} nodes)
            ;; what it needs first, before its topic: with few passages, those claims are
            ;; what connects it to the rest
            _     (prereq/judge! conn @client nil {:models models})
            {:keys [successors]} (cluster/refresh! conn (cluster/llm-namer @client opts))]
        (study/clusters-replaced! conn successors)
        {:concept/id id :mentions (count nodes)}))))

(defn remove-concept!
  "Concept `id` is no concept: it goes (its names ignored from then on), with
  the background only it needed, and leaves its topic. Returns true, nil when there is no such concept."
  [conn id]
  (when (d/entid (d/db conn) [:concept/id id])
    (cluster/remove-concepts! conn [id])
    true))

(defn remove-topic!
  "Topic (cluster) `id` is no knowledge: it and its concepts go (their names
  ignored from then on) with the background only they needed, and so does
  learning it as a goal. Returns
  {:cluster/id :concepts n}, nil when there is no such topic."
  [conn id]
  (store/goal! conn (study/cluster-scope id) false)
  (when-let [removed (cluster/remove! conn id)]
    {:cluster/id id :concepts (count removed)}))
