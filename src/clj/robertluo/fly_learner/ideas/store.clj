(ns robertluo.fly-learner.ideas.store
  "Ideas (robertluo.fly-learner.ideas) in Datalevin. Anchors and links are
  stored as plain values, opaque here. `schema` is merged into the
  database's at the composition root."
  (:require [datalevin.core :as d]))

(def schema
  {:idea/id      {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :idea/stance  {:db/valueType :db.type/keyword}
   :idea/text    {:db/valueType :db.type/string}
   :idea/anchors {}                                    ; [anchor ..] (EDN)
   :idea/links   {}                                    ; [idea-id ..] (EDN)
   :idea/at      {:db/valueType :db.type/instant}})

(defn save!
  "Store a new idea (see robertluo.fly-learner.ideas/idea; :idea/at epoch ms)."
  [conn idea]
  (d/transact! conn [(update idea :idea/at #(java.util.Date. (long %)))])
  nil)

(defn delete!
  "Remove idea `id`; true when it existed. Links to it from other ideas
  are left, and read as nothing (robertluo.fly-learner.ideas/links)."
  [conn id]
  (when-let [e (d/entid (d/db conn) [:idea/id id])]
    (d/transact! conn [[:db/retractEntity e]])
    true))

(defn all
  "Every idea, oldest first, :idea/at in epoch ms."
  [db]
  (->> (d/q '[:find [(pull ?e [:idea/id :idea/stance :idea/text :idea/anchors :idea/links :idea/at]) ...]
              :where [?e :idea/id]]
            db)
       (map #(update % :idea/at (fn [^java.util.Date d] (.getTime d))))
       (sort-by :idea/at)
       vec))
