(ns robertluo.fly-learner.learning.concepts
  "The concept graph as a robertluo.fly-learner.learning/Curriculum. The one
  place where learning meets knowledge: nothing else in learning (or in the
  features built on it) reads knowledge directly.

  Items:
    [:concept id]       knowing a concept; weight = passages mentioning it
    [:relation a b]     knowing how two concepts connect (a < b as strings):
                        a broader link (weight 2), or concepts related by
                        passages they share (weight = shared passages, two
                        at least; rarer pairs are noise)
  Prerequisites: a concept's broader concepts; a relation's two concepts."
  (:require [datalevin.core :as d]
            [robertluo.fly-learner.concept.graph :as graph]
            [robertluo.fly-learner.concept.view :as view]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.learning :as learning]))

(defn concept-item [id] [:concept id])

(defn relation-item [a b]
  (let [[a b] (sort-by str [a b])] [:relation a b]))

(defn concepts-items
  "Items about the concepts `ids` (a set): each concept, and every relation
  touching one of them. What learning a cluster of concepts covers."
  [ids items]
  (into #{} (filter (fn [[kind a b]]
                      (case kind
                        :concept  (contains? ids a)
                        :relation (or (contains? ids a) (contains? ids b))
                        false)))
        items))

^:rct/test
(comment
  (concepts-items #{:a} [[:concept :a] [:concept :b] [:relation :a :b] [:relation :b :c]])
  ;=> #{[:concept :a] [:relation :a :b]}
  )

(def ^:private source-limit 5)

(def ^:private passage-pull [:db/id :node/title :node/summary :node/text :node/level :node/page])

(defn- passages
  "Up to `source-limit` passages (node ids), overview first, as material sources."
  [db node-ids]
  (->> node-ids
       (map #(d/pull db passage-pull [:node/id %]))
       (sort-by (juxt :node/level :node/page))
       (take source-limit)
       (mapv (fn [n]
               {:title (->> (conj (mapv :node/title (rest (db/ancestors-of db (:db/id n)))) (:node/title n))
                            (interpose " › ") (apply str))
                :text  (or (:node/summary n) (:node/text n) "")}))))

(defn curriculum
  "Curriculum over the concept graph in `db` (a snapshot: build it again
  after the graph changes)."
  [db]
  (let [g        (graph/load-graph db)
        adj      (graph/edges g)
        concepts (:concepts g)
        label    #(get-in concepts [% :concept/label])
        rels     (->> (:edges (view/graph db))
                      (keep (fn [{:keys [from to kind shared]}]
                              (cond
                                (= :broader kind) [(relation-item from to) {:weight 2 :narrower from :broader to}]
                                (<= 2 shared)     [(relation-item from to) {:weight shared}])))
                      (into {}))
        weights  (into (into {} (for [id (keys concepts)]
                                  [(concept-item id) (max 1 (count (get-in g [:mentions id])))]))
                       (map (fn [[item r]] [item (:weight r)]))
                       rels)]
    (reify learning/Curriculum
      (items [_] weights)

      (prerequisites [_ [kind a b]]
        (case kind
          :concept  (mapv concept-item (get-in concepts [a :concept/broader]))
          :relation [(concept-item a) (concept-item b)]
          []))

      (material [_ [kind a b]]
        (case kind
          :concept
          (let [{:concept/keys [gloss aliases]} (concepts a)]
            {:title   (label a)
             :about   (str "the concept \"" (label a) "\""
                           (when gloss (str ": " gloss))
                           (when-let [others (seq (disj aliases (label a)))]
                             (str " (also written " (apply str (interpose ", " (sort others))) ")")))
             :sources (passages db (get-in g [:mentions a]))})

          :relation
          (let [{:keys [narrower broader]} (rels [kind a b])
                gloss  #(when-let [gl (get-in concepts [% :concept/gloss])] (str " (" gl ")"))
                shared (get-in adj [a b :nodes])]
            {:title   (str (label a) " — " (label b))
             :about   (if narrower
                        (str "how \"" (label narrower) "\"" (gloss narrower) " is a kind or part of \""
                             (label broader) "\"" (gloss broader))
                        (str "how \"" (label a) "\"" (gloss a) " and \"" (label b) "\"" (gloss b)
                             " relate, as the sources discuss them together"))
             :sources (passages db (if (seq shared)
                                     shared
                                     ;; a broader link need not share passages
                                     (concat (take 2 (get-in g [:mentions a]))
                                             (take 2 (get-in g [:mentions b])))))})
          nil)))))
