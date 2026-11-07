(ns robertluo.fly-learner.concept.view
  "Concepts for display (robertluo.fly-learner.server.api :concepts)."
  (:require [datalevin.core :as d]
            [robertluo.fly-learner.concept.graph :as graph]
            [robertluo.fly-learner.concept.prereq :as prereq]
            [robertluo.fly-learner.db :as db]))

(defn- ref-of
  "A concept as referenced from elsewhere: id, label, gloss, mention count."
  [g id]
  (-> (get-in g [:concepts id])
      (select-keys [:concept/id :concept/label :concept/gloss :concept/origin])
      (assoc :concept/mentions (count (get-in g [:mentions id])))))

(defn- refs [g ids]
  (->> ids (map #(ref-of g %)) (sort-by :concept/label) vec))

(defn index
  "Every concept as a ref, by label."
  [db]
  (let [g (graph/load-graph db)]
    (refs g (keys (:concepts g)))))

(def related-limit 12)

(defn- related
  "Strongest co-occurring concepts. Pairs sharing one passage are noisy, so
  they are shown only when nothing is shared more often."
  [adj id]
  (let [rel    (->> (adj id)
                    (filter #(= :related (:kind (val %))))
                    (sort-by (comp - :weight val)))
        strong (filter #(<= 2 (count (:nodes (val %)))) rel)]
    (take related-limit (if (seq strong) strong rel))))

(def ^:private mention-pull
  [:db/id :node/id :node/title :node/kind :node/level :node/page :node/order :node/summary :node/text])

(def needed-by-limit 24)

(defn concept
  "Concept `id` for its page: label, gloss, other names, who wrote it,
  :concept/origin (:inferred: hidden background), broader / narrower /
  related concepts (related with the number of passages they share), what
  to learn first (:concept/needs the direct ones, each with :why;
  :concept/before every one, each after what it needs), what it leads to
  (:concept/needed-by, by label, at most `needed-by-limit`, with
  :concept/needed-by-count), and the passages mentioning it with their
  paths, overview first. nil when not found."
  [db id]
  (let [g                 (graph/load-graph db)
        {:keys [dag why]} (prereq/derived db)
        label             #(get-in g [:concepts % :concept/label])]
    (when-let [c (get-in g [:concepts id])]
      (let [adj      (graph/edges g)
            needed-by (for [[b as] dag :when (contains? as id)] b)
            narrower (for [[cid x] (:concepts g) :when (contains? (:concept/broader x) id)] cid)
            mentions (->> (get-in g [:mentions id])
                          (map #(d/pull db mention-pull [:node/id %]))
                          (sort-by (juxt :node/level :node/page :node/order))
                          (mapv (fn [n]
                                  (-> n
                                      (assoc :node/path (mapv #(select-keys % [:node/id :node/title])
                                                              (db/ancestors-of db (:db/id n))))
                                      (dissoc :db/id)))))]
        (-> (select-keys c [:concept/id :concept/label :concept/gloss :concept/by :concept/origin])
            (assoc :concept/aliases  (vec (sort (disj (:concept/aliases c) (:concept/label c))))
                   :concept/broader  (refs g (:concept/broader c))
                   :concept/narrower (refs g narrower)
                   :concept/related  (vec (for [[cid e] (related adj id)]
                                            (assoc (ref-of g cid) :shared (count (:nodes e)))))
                   :concept/needs    (vec (for [a (sort-by label (dag id))]
                                            (assoc (ref-of g a) :why (why [id a]))))
                   :concept/before   (mapv #(ref-of g %) (prereq/before dag id (constantly false) label))
                   :concept/needed-by-count (count needed-by)
                   :concept/needed-by (refs g (take needed-by-limit (sort-by label needed-by)))
                   :concept/mentions mentions))))))

(defn graph
  "The whole concept graph, for the client to expand and collapse locally:
  {:concepts [ref ..] :edges [{:from :to :kind ..} ..]}.
    :broader  :from the narrower concept :to its broader one
    :related  each concept's strongest co-occurrences (see `related`), once
              per pair, with :weight (0, 1] and :shared passages
    :requires :from a concept :to one to learn before it, with :why"
  [db]
  (let [g   (graph/load-graph db)
        adj (graph/edges g)
        ids (keys (:concepts g))
        {:keys [dag why]} (prereq/derived db)]
    {:concepts (refs g ids)
     :edges    (vec (concat
                     (for [id ids
                                p  (get-in g [:concepts id :concept/broader])
                                :when (get-in g [:concepts p])]
                            {:from id :to p :kind :broader})
                     (->> ids
                          (mapcat (fn [id] (for [[cid e] (related adj id)] [(sort [id cid]) e])))
                          (into {})
                          (map (fn [[[a b] e]] {:from a :to b :kind :related
                                                :weight (:weight e) :shared (count (:nodes e))})))
                     (for [[b as] dag, a as :when (and (get-in g [:concepts b]) (get-in g [:concepts a]))]
                       {:from b :to a :kind :requires :why (why [b a])})))}))
