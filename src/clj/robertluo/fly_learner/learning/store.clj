(ns robertluo.fly-learner.learning.store
  "The evidence log of robertluo.fly-learner.learning, in Datalevin. Only
  evidence is stored; states and scores are derived from it. Also the
  learner's goals: the parts of the curriculum they chose to learn now, as
  opaque scope ids (what a scope covers is decided elsewhere).
  `schema` is merged into the database's at the composition root
  (robertluo.fly-learner.server.system)."
  (:require [datalevin.core :as d]))

(def schema
  {:evidence/item   {}                                 ; item id (EDN value), opaque to the store
   :evidence/kind   {:db/valueType :db.type/keyword}
   :evidence/grade  {:db/valueType :db.type/double}
   :evidence/at     {:db/valueType :db.type/instant}
   :evidence/source {}                                 ; what produced it, e.g. a question id
   :goal/key        {:db/valueType :db.type/string :db/unique :db.unique/identity} ; pr-str of the scope
   :goal/scope      {}                                 ; scope id (EDN value), opaque to the store
   :goal/at         {:db/valueType :db.type/instant}})

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

(defn goals
  "Scopes the learner chose to learn now, in the order chosen."
  [db]
  (->> (d/q '[:find [(pull ?g [:goal/scope :goal/at]) ...] :where [?g :goal/key]] db)
       (sort-by :goal/at)
       (mapv :goal/scope)))

(defn goal!
  "Learn `scope` now (`on?` true), or stop."
  [conn scope on?]
  (d/transact! conn (if on?
                      [{:goal/key (pr-str scope) :goal/scope scope :goal/at (java.util.Date.)}]
                      (if (d/entid (d/db conn) [:goal/key (pr-str scope)])
                        [[:db/retractEntity [:goal/key (pr-str scope)]]]
                        [])))
  nil)
