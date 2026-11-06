(ns robertluo.fly-learner.db
  "Datalevin storage for sources, the folded knowledge DAG, questions and
  the learner's progress."
  (:require [datalevin.core :as d]))

(def schema
  {;; sources -----------------------------------------------------------
   :source/id       {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :source/type     {:db/valueType :db.type/keyword} ; :pdf, later :web, :epub ...
   :source/title    {:db/valueType :db.type/string}
   :source/path     {:db/valueType :db.type/string}
   :source/root     {:db/valueType :db.type/ref}     ; level-0 birdview node

   ;; knowledge nodes ---------------------------------------------------
   :node/id         {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :node/source     {:db/valueType :db.type/ref}
   :node/level      {:db/valueType :db.type/long}
   :node/kind       {:db/valueType :db.type/keyword} ; :source :section :group :paragraph
   ;; :db/fulltext attrs are searched by robertluo.fly-learner.ask; after adding one, existing
   ;; data needs a (copy! dir new-dir) once
   :node/title      {:db/valueType :db.type/string :db/fulltext true}
   :node/summary    {:db/valueType :db.type/string :db/fulltext true}
   :node/text       {:db/valueType :db.type/string :db/fulltext true}
   :node/keywords   {:db/valueType :db.type/string :db/cardinality :db.cardinality/many :db/fulltext true}
   :node/page       {:db/valueType :db.type/long}
   :node/order      {:db/valueType :db.type/long}
   :node/children   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :node/links      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}

   ;; asks: questions to the knowledge base and their answers (robertluo.fly-learner.ask)
   :ask/id          {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :ask/question    {:db/valueType :db.type/string}
   :ask/title       {:db/valueType :db.type/string}
   :ask/answer      {:db/valueType :db.type/string} ; prose citing [n] = (nth :ask/cited (dec n))
   :ask/cited       {}                               ; [node-id ...] (EDN value)
   :ask/matches     {}                               ; [node-id ...] retrieved, not cited
   :ask/followups   {}                               ; [question ...]
   :ask/model       {:db/valueType :db.type/string}
   :ask/at          {:db/valueType :db.type/instant}

   ;; questions ---------------------------------------------------------
   :question/id     {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :question/node   {:db/valueType :db.type/ref}
   :question/kind   {:db/valueType :db.type/keyword}
   :question/prompt {:db/valueType :db.type/string}
   :question/answer {:db/valueType :db.type/string}

   ;; progress ----------------------------------------------------------
   :attempt/question {:db/valueType :db.type/ref}
   :attempt/response {:db/valueType :db.type/string}
   :attempt/correct? {:db/valueType :db.type/boolean}
   :attempt/at       {:db/valueType :db.type/instant}

   ;; spaced-repetition state, one per node the learner is studying
   :review/node     {:db/valueType :db.type/ref :db/unique :db.unique/identity}
   :review/ease     {:db/valueType :db.type/double}
   :review/interval {:db/valueType :db.type/long}   ; days
   :review/due      {:db/valueType :db.type/instant}})

(defn connect
  "Open (or create) the database at `dir`."
  [dir]
  (d/get-conn dir schema))

(defn close [conn]
  (d/close conn))

(defn copy!
  "Copy every datom of the database at `from` into a new database at `to`
  under the current schema, so attributes newly marked :db/fulltext get
  indexed. Ids (:node/id, :ask/id ..) are kept. Only while nothing else has
  `from` open; then swap the directories.
  (datalevin.core/re-index is not used: on 1.2.10 it dropped most of the
  full-text index.)"
  [from to]
  (let [src  (d/get-conn from)
        dst  (d/get-conn to schema)
        ref? #(= :db.type/ref (get-in schema [% :db/valueType]))]
    (try
      ;; one transaction: refs are tempids, resolved within it
      (d/transact! dst (vec (for [{:keys [e a v]} (d/datoms (d/db src) :eav)]
                              [:db/add (str e) a (if (ref? a) (str v) v)])))
      (count (d/datoms (d/db dst) :eav))
      (finally (d/close src) (d/close dst)))))

(def ^:private node-ref
  "Shape of a node when referenced from another node."
  [:node/id :node/title :node/kind :node/level :node/order :node/page :node/summary :node/text])

(defn- ancestors-of
  "Parent chain of entity `eid`, root first, as node refs."
  [db eid]
  (loop [eid eid, acc ()]
    (if-let [parent (first (:node/_children (d/pull db [{:node/_children [:db/id]}] eid)))]
      (let [pid (:db/id parent)]
        (recur pid (conj acc (d/pull db [:node/id :node/title :node/kind] pid))))
      (vec acc))))

(defn node
  "A node for display: its own fields, children and links (as node refs),
  ancestors (root first) and its source. nil when not found."
  [db node-id]
  (when-let [eid (d/entid db [:node/id node-id])]
    (let [n (d/pull db [:node/id :node/kind :node/level :node/order :node/title :node/summary
                        :node/text :node/keywords :node/page
                        {:node/children node-ref} {:node/links node-ref}
                        {:node/source [:source/id :source/title]}]
                    eid)]
      (-> n
          (update :node/children #(vec (sort-by :node/order %)))
          (update :node/links #(vec (sort-by (juxt :node/level :node/title) %)))
          (update :node/keywords #(vec (sort %)))
          (assoc :node/ancestors (ancestors-of db eid))))))

(defn sources [db]
  (->> (d/q '[:find [(pull ?s [:source/id :source/title :source/type
                               {:source/root [:node/id :node/summary]}]) ...]
              :where [?s :source/id]]
            db)
       (sort-by :source/title)
       vec))

(defn source [db source-id]
  (d/pull db [:source/id :source/title :source/type :source/path {:source/root [:node/id :node/summary]}]
          [:source/id source-id]))

(defn nodes-with-keyword
  "Summarised nodes carrying keyword `kw`, highest level first."
  [db kw]
  (->> (d/q '[:find [(pull ?n [:node/id :node/title :node/kind :node/level :node/page :node/summary]) ...]
              :in $ ?kw
              :where [?n :node/keywords ?kw]]
            db kw)
       (sort-by (juxt :node/level :node/title))
       vec))
