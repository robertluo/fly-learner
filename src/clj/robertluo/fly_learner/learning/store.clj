(ns robertluo.fly-learner.learning.store
  "The evidence log of robertluo.fly-learner.learning, in Datalevin. Only
  evidence is stored; states and scores are derived from it.
  `schema` is merged into the database's at the composition root
  (robertluo.fly-learner.server.system)."
  (:require [datalevin.core :as d]))

(def schema
  {:evidence/item   {}                                 ; item id (EDN value), opaque to the store
   :evidence/kind   {:db/valueType :db.type/keyword}
   :evidence/grade  {:db/valueType :db.type/double}
   :evidence/at     {:db/valueType :db.type/instant}
   :evidence/source {}})                               ; what produced it, e.g. a question id

(defn- ->datoms [{:keys [item kind grade at source]}]
  (cond-> {:evidence/item item :evidence/kind kind :evidence/at (java.util.Date. (long at))}
    grade  (assoc :evidence/grade (double grade))
    source (assoc :evidence/source source)))

(defn record!
  "Append evidence (a map, see robertluo.fly-learner.learning, or a seq of them)."
  [conn evidence]
  (d/transact! conn (mapv ->datoms (if (map? evidence) [evidence] evidence)))
  nil)

(defn evidence
  "Every piece of evidence, oldest first, :at in epoch ms."
  [db]
  (->> (d/q '[:find [(pull ?e [:evidence/item :evidence/kind :evidence/grade :evidence/at :evidence/source]) ...]
              :where [?e :evidence/kind]]
            db)
       (map (fn [{:evidence/keys [item kind grade at source]}]
              (cond-> {:item item :kind kind :at (.getTime ^java.util.Date at)}
                grade  (assoc :grade grade)
                source (assoc :source source))))
       (sort-by :at)
       vec))
