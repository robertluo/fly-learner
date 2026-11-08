(ns migrations.source-identity
  "One-off (2026-11-08): sources become their document, not their path.

  Before, a source was found again by :source/path and got a new random
  :source/id on each ingest; exercise ids hashed the path; exploration
  anchors carried the path. Now (robertluo.fly-learner.ingest/same-source)
  a source is found by its content hash, and its id is kept. This brings a
  db made before up to that:
    - each source gets :source/hash from its file (its id is kept)
    - each exercise gets the id derived from its source's id; the authored
      questions with the old id, and the evidence of answers to them, follow
    - exploration anchors and cites name the source by id, not path

    (plan (d/db conn))      what would change, transacting nothing
    (migrate! conn)         do it, in one transaction"
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]
            [robertluo.fly-learner.exercises :as exercises]
            [robertluo.fly-learner.ingest :as ingest]))

(defn- old-exercise-id [path label]
  (java.util.UUID/nameUUIDFromBytes (.getBytes (str path "#exercise-" label) "UTF-8")))

(defn- re-anchor [by-path a]
  (if-let [p (:source/path a)]
    (-> a (dissoc :source/path) (assoc :source/id (by-path p)))
    a))

(defn plan
  "{:hashes [[source-id hash] ..] :missing [path ..] :exercises {old new}
  :tx [..]} for db `db`."
  [db]
  (let [sources  (d/q '[:find ?e ?id ?p :where [?e :source/id ?id] [?e :source/path ?p]] db)
        by-path  (into {} (map (fn [[_ id p]] [p id])) sources)
        hashes   (for [[e id p] sources :when (.isFile (io/file p))
                       :when (not (:source/hash (d/pull db [:source/hash] e)))]
                   [e id (ingest/file-hash p)])
        xs       (d/q '[:find ?x ?xid ?label ?sid ?p
                        :where [?x :exercise/id ?xid] [?x :exercise/label ?label] [?x :exercise/source ?s]
                        [?s :source/id ?sid] [?s :source/path ?p]]
                      db)
        renamed  (into {} (for [[_ xid label sid p] xs
                                :when (= xid (old-exercise-id p label))]
                            [xid (exercises/exercise-id sid label)]))
        x-eid    (into {} (map (fn [[x xid]] [xid x])) xs)
        q-eid    #(d/entid db [:question/id %])
        evidence (d/q '[:find ?e ?src :where [?e :evidence/source ?src]] db)
        explorations (d/q '[:find ?e ?a ?c :where [?e :exploration/id] [?e :exploration/anchor ?a]
                            [(get-else $ ?e :exploration/cites []) ?c]]
                          db)]
    {:hashes    (mapv (fn [[_ id h]] [id h]) hashes)
     :missing   (vec (for [[_ _ p] sources :when (not (.isFile (io/file p)))] p))
     :exercises renamed
     :tx        (vec (concat
                      (for [[e _ h] hashes] [:db/add e :source/hash h])
                      (for [[old new] renamed] [:db/add (x-eid old) :exercise/id new])
                      (for [[old new] renamed :let [q (q-eid old)] :when q] [:db/add q :question/id new])
                      (for [[e src] evidence :let [new (renamed src)] :when new] [:db/add e :evidence/source new])
                      (for [[e a c] explorations
                            :let [a' (re-anchor by-path a) c' (mapv #(re-anchor by-path %) c)]
                            tx [(when (not= a a') [:db/add e :exploration/anchor a'])
                                (when (not= c c') [:db/add e :exploration/cites c'])]
                            :when tx]
                        tx)))}))

(defn migrate!
  "Bring the db of `conn` up to sources known by content. Returns the plan done."
  [conn]
  (d/update-schema conn {:source/hash {:db/valueType :db.type/string :db/unique :db.unique/identity}})
  (let [p (plan (d/db conn))]
    (when (seq (:tx p)) (d/transact! conn (:tx p)))
    (dissoc p :tx)))
