(ns robertluo.fly-learner.explore.store
  "Explorations (robertluo.fly-learner.explore) and proposals for them, in
  Datalevin. What they are about (a node) is an opaque anchor here, given
  by robertluo.fly-learner.explore.knowledge. `schema` is merged into the
  database's at the composition root.

    exploration  a spec kept for later: {:exploration/id :title :spec :anchor
                 :cites [anchor ..] :pitch :feedback :previous :model :cost :at}
    proposal     what a passage could become, or :none: {:proposal/node
                 :kind :pitch :model :at}; one per node considered"
  (:require [clojure.edn :as edn]
            [datalevin.core :as d]))

(def schema
  {:exploration/id       {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :exploration/title    {:db/valueType :db.type/string}
   :exploration/spec     {:db/valueType :db.type/string}  ; EDN of the normalized spec
   :exploration/anchor   {}                               ; what it is about (EDN, see explore.knowledge)
   :exploration/cites    {}                               ; [anchor ..]: [n] in its text is the n-th
   :exploration/pitch    {:db/valueType :db.type/string}  ; what was asked for
   :exploration/feedback {:db/valueType :db.type/string}  ; the learner's request for this version
   :exploration/previous {:db/valueType :db.type/uuid}    ; the version it remakes
   :exploration/model    {:db/valueType :db.type/string}
   :exploration/cost     {:db/valueType :db.type/double}  ; USD, approximate
   :exploration/at       {:db/valueType :db.type/instant}

   :proposal/node        {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :proposal/kind        {:db/valueType :db.type/keyword} ; main element type, or :none
   :proposal/pitch       {:db/valueType :db.type/string}
   :proposal/model       {:db/valueType :db.type/string}
   :proposal/at          {:db/valueType :db.type/instant}})

(defn- read-spec [s] (edn/read-string s))

(defn save!
  "Store exploration `x` (:exploration/spec a map); returns its id."
  [conn x]
  (let [id (or (:exploration/id x) (random-uuid))]
    (d/transact! conn [(-> x
                           (assoc :exploration/id id :exploration/at (java.util.Date.))
                           (update :exploration/spec pr-str))])
    id))

(def ^:private pull-all
  [:exploration/id :exploration/title :exploration/spec :exploration/anchor :exploration/cites
   :exploration/pitch :exploration/feedback :exploration/previous :exploration/model
   :exploration/cost :exploration/at])

(defn by-id [db id]
  (when-let [e (d/entid db [:exploration/id id])]
    (update (d/pull db pull-all e) :exploration/spec read-spec)))

(defn all
  "Every exploration without its spec, newest first; versions remade into
  another left out unless `versions?`."
  ([db] (all db false))
  ([db versions?]
   (let [xs (d/q '[:find [(pull ?e [:exploration/id :exploration/title :exploration/anchor
                                    :exploration/pitch :exploration/previous :exploration/at]) ...]
                   :where [?e :exploration/id]]
                 db)
         remade (set (keep :exploration/previous xs))]
     (->> xs
          (remove #(and (not versions?) (remade (:exploration/id %))))
          (sort-by :exploration/at #(compare %2 %1))
          vec))))

(defn delete! [conn id]
  (when-let [e (d/entid (d/db conn) [:exploration/id id])]
    (d/transact! conn [[:db/retractEntity e]])
    true))

(defn save-proposals!
  "Store proposals [{:proposal/node id :proposal/kind kw :proposal/pitch ..}]."
  [conn proposals model]
  (let [at (java.util.Date.)]
    (when (seq proposals)
      (d/transact! conn (mapv #(assoc % :proposal/model model :proposal/at at) proposals)))))

(defn proposals
  "node-id -> proposal for every node considered (:proposal/kind :none for
  one judged not worth exploring)."
  [db]
  (into {}
        (map (juxt :proposal/node identity))
        (d/q '[:find [(pull ?p [:proposal/node :proposal/kind :proposal/pitch]) ...]
               :where [?p :proposal/node]]
             db)))

(defn proposal [db node-id]
  (when-let [e (d/entid db [:proposal/node node-id])]
    (d/pull db [:proposal/node :proposal/kind :proposal/pitch] e)))

(defn forget-proposals!
  "Remove the proposals for `node-ids` (nodes that are gone)."
  [conn node-ids]
  (when (seq node-ids)
    (d/transact! conn (mapv (fn [id] [:db/retractEntity [:proposal/node id]]) node-ids))))
