(ns robertluo.fly-learner.db
  "Datalevin storage for knowledge: sources, the folded knowledge DAG,
  concepts and asks. Layers above (learning, quiz) bring their own schema;
  they are merged at the composition root (robertluo.fly-learner.server.system)."
  (:require [datalevin.core :as d]))

(def schema
  {;; sources -----------------------------------------------------------
   :source/id       {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :source/type     {:db/valueType :db.type/keyword} ; :pdf, later :web, :epub ...
   :source/title    {:db/valueType :db.type/string}
   :source/path     {:db/valueType :db.type/string}  ; where the file is now (may move)
   :source/hash     {:db/valueType :db.type/string :db/unique :db.unique/identity} ; SHA-256 of the file's bytes
   :source/root     {:db/valueType :db.type/ref}     ; level-0 birdview node
   :source/exercises-by {:db/valueType :db.type/string} ; its exercises were looked for, by this model

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
   ;; key sentences of a passage (robertluo.fly-learner.keypoints): [{:para node-id :quote :label}], [] for none
   :node/keypoints  {}

   ;; concepts: ideas with an identity of their own, across sources
   ;; (robertluo.fly-learner.concept). Node n mentions concept c when one of
   ;; n's :node/keywords is an alias of c.
   :concept/id      {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :concept/label   {:db/valueType :db.type/string}
   ;; the keyword strings naming it; unique: a keyword names at most one concept
   :concept/aliases {:db/valueType :db.type/string :db/cardinality :db.cardinality/many
                     :db/unique :db.unique/value}
   :concept/gloss   {:db/valueType :db.type/string} ; one sentence fixing its sense
   :concept/broader {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many} ; is-a / part-of
   :concept/by      {:db/valueType :db.type/string} ; last asserted by: a model id or "human"
   :ignored/keyword {:db/valueType :db.type/string :db/unique :db.unique/identity} ; names no concept
   ;; :inferred: background the sources assume, named by a model (robertluo.fly-learner.concept.prereq)
   :concept/origin     {:db/valueType :db.type/keyword}
   :concept/prereqs-by {:db/valueType :db.type/string} ; its prerequisites were judged, by this model

   ;; prerequisites: a claim that a concept is best grasped before another
   :prereq/of       {:db/valueType :db.type/ref}     ; the concept that needs ..
   :prereq/needs    {:db/valueType :db.type/ref}     ; .. this one first
   :prereq/why      {:db/valueType :db.type/string}
   :prereq/by       {:db/valueType :db.type/string}  ; a model id or "human"

   ;; exercises the author wrote, with their solutions (robertluo.fly-learner.exercises)
   :exercise/id       {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :exercise/source   {:db/valueType :db.type/ref}
   :exercise/label    {:db/valueType :db.type/string} ; "1.5", as the book numbers it
   :exercise/node     {:db/valueType :db.type/ref}    ; the paragraph it starts in
   :exercise/page     {:db/valueType :db.type/long}
   :exercise/text     {:db/valueType :db.type/string}
   :exercise/solution {:db/valueType :db.type/string} ; the author's
   :exercise/refers   {}                              ; [label ..] exercises it builds on
   :exercise/summary  {:db/valueType :db.type/string} ; what it asks, in a line
   :exercise/points   {}                              ; [key point ..] a full answer covers
   :exercise/concepts {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many} ; what it tests
   :exercise/by       {:db/valueType :db.type/string} ; the model that linked it

   ;; edits: every change to concepts, by a model or a human, in order
   :edit/op         {:db/valueType :db.type/keyword}
   :edit/args       {}                               ; EDN: enough to replay the edit
   :edit/by         {:db/valueType :db.type/string}
   :edit/at         {:db/valueType :db.type/instant}

   ;; clusters: the concept graph's topics (robertluo.fly-learner.concept.cluster)
   :cluster/id       {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :cluster/label    {:db/valueType :db.type/string}
   :cluster/gloss    {:db/valueType :db.type/string}
   :cluster/concepts {}                              ; [concept-id ...] (EDN value)
   :cluster/by       {:db/valueType :db.type/string} ; the model that named it
   :cluster/at       {:db/valueType :db.type/instant}

   ;; asks: questions to the knowledge base and their answers (robertluo.fly-learner.ask)
   :ask/id          {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :ask/question    {:db/valueType :db.type/string}
   :ask/title       {:db/valueType :db.type/string}
   :ask/answer      {:db/valueType :db.type/string} ; prose citing [n] = (nth :ask/cited (dec n))
   :ask/cited       {}                               ; [node-id ...] (EDN value)
   :ask/matches     {}                               ; [node-id ...] retrieved, not cited
   :ask/followups   {}                               ; [question ...]
   :ask/model       {:db/valueType :db.type/string}
   :ask/quote       {:db/valueType :db.type/string} ; the passage asked about, if any
   :ask/focus       {}                               ; [node-id ...] the passage is in
   :ask/at          {:db/valueType :db.type/instant}})

(defn connect
  "Open (or create) the database at `dir`, with the knowledge `schema` and
  any `more` schemas (from other layers)."
  [dir & more]
  (d/get-conn dir (apply merge schema more)))

(defn close [conn]
  (d/close conn))

(defn per-version
  "`f` of a db, computed once per db version (its store and last tx) and
  kept until the next, under a lock: for heavy reads that pages share.
  The lock matters too: Datalevin 1.2.10 can fail a heavy query
  (\"Comparison method violates its general contract\", in its tuple
  sort) when two threads run it at once."
  [f]
  (let [cache (atom nil), lock (Object.)]
    (fn [db]
      (let [store (:store db), tx (:max-tx db)
            hit   #(let [c @cache] (when (and c (identical? store (:store c)) (= tx (:tx c))) [(:value c)]))]
        (first (or (hit)
                   (locking lock
                     (or (hit)
                         (let [v (f db)]
                           (reset! cache {:store store :tx tx :value v})
                           [v])))))))))

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

(defn ancestors-of
  "Parent chain of entity `eid`, root first: [{:node/id :node/title :node/kind} ..]."
  [db eid]
  (loop [eid eid, acc ()]
    (if-let [parent (first (:node/_children (d/pull db [{:node/_children [:db/id]}] eid)))]
      (let [pid (:db/id parent)]
        (recur pid (conj acc (d/pull db [:node/id :node/title :node/kind] pid))))
      (vec acc))))

(defn- node-concepts
  "Concepts node `eid` mentions (one of its keywords is their alias), by label."
  [db eid]
  (->> (d/q '[:find [(pull ?c [:concept/id :concept/label]) ...]
              :in $ ?n :where [?n :node/keywords ?k] [?c :concept/aliases ?k]]
            db eid)
       (sort-by :concept/label)
       vec))

(defn- covered-keywords [db eid]
  (d/q '[:find [?k ...] :in $ ?n :where [?n :node/keywords ?k] [_ :concept/aliases ?k]] db eid))

(defn node
  "A node for display: its own fields, children and links (as node refs),
  ancestors (root first), its source, the concepts it mentions, and the
  keywords that name no concept. nil when not found."
  [db node-id]
  (when-let [eid (d/entid db [:node/id node-id])]
    (let [n (d/pull db [:node/id :node/kind :node/level :node/order :node/title :node/summary
                        :node/text :node/keywords :node/page :node/keypoints
                        {:node/children node-ref} {:node/links node-ref}
                        {:node/source [:source/id :source/title]}]
                    eid)]
      (-> n
          (update :node/children #(vec (sort-by :node/order %)))
          (update :node/links #(vec (sort-by (juxt :node/level :node/title) %)))
          (assoc :node/concepts (node-concepts db eid))
          ;; keywords naming a concept are shown as the concept
          (update :node/keywords #(vec (sort (remove (set (covered-keywords db eid)) %))))
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

(defn nodes-on-page
  "Ids of the paragraphs of source `source-id` on PDF page `page`."
  [db source-id page]
  (d/q '[:find [?id ...]
         :in $ ?sid ?p
         :where [?s :source/id ?sid] [?n :node/source ?s] [?n :node/page ?p]
         [?n :node/kind :paragraph] [?n :node/id ?id]]
       db source-id page))

(defn- paragraphs-at
  "[[order eid] ..] of the paragraphs of source entity `s` starting on page `p`."
  [db s p]
  (d/q '[:find ?o ?n :in $ ?s ?p
         :where [?n :node/source ?s] [?n :node/page ?p] [?n :node/kind :paragraph] [?n :node/order ?o]]
       db s p))

(defn node-at-page
  "The node to show beside page `page` of source `source-id`, reading from
  node `near-id`: `near-id` itself while any of its text is on the page (a
  paragraph starting there, or the last one of an earlier page, which may run
  over); else, among the nodes with a paragraph starting on the page, the
  first in reading order of `near-id`'s kind and level, else of its kind at
  the nearest level; else `near-id`. A page with no paragraph starting on it
  (a figure, a long paragraph) is read through the one running over."
  [db source-id page near-id]
  (let [s    (d/entid db [:source/id source-id])
        near (d/pull db [:db/id :node/kind :node/level] [:node/id near-id])
        with-ancestors (fn [paras]
                         (->> (sort paras) (map second)
                              (mapcat (fn [e] (conj (vec (keep #(d/entid db [:node/id (:node/id %)]) (ancestors-of db e))) e)))
                              distinct
                              (map #(d/pull db [:db/id :node/id :node/kind :node/level :node/order] %))))
        on   (paragraphs-at db s page)
        prev (some #(not-empty (paragraphs-at db s %)) (range (dec page) (max 0 (- page 12)) -1))
        over (when (seq prev) [(apply max-key first prev)])
        kin  (filter #(= (:node/kind near) (:node/kind %)) (with-ancestors (or (not-empty on) over)))
        by-order #(first (sort-by :node/order %))]
    (cond
      (or (nil? s) (nil? (:db/id near)))                                       nil
      (some #(= (:db/id near) (:db/id %)) (with-ancestors (concat over on)))  near-id
      :else (or (:node/id (or (by-order (filter #(= (:node/level near) (:node/level %)) kin))
                              (first (sort-by (juxt #(abs (- (:node/level %) (:node/level near))) :node/order) kin))))
                near-id))))

(defn nodes-with-keyword
  "Summarised nodes carrying keyword `kw`, highest level first."
  [db kw]
  (->> (d/q '[:find [(pull ?n [:node/id :node/title :node/kind :node/level :node/page :node/summary]) ...]
              :in $ ?kw
              :where [?n :node/keywords ?kw]]
            db kw)
       (sort-by (juxt :node/level :node/title))
       vec))
