(ns robertluo.fly-learner.concept.cluster
  "Clusters: the concept graph's topics, so a growing body of knowledge can
  be seen whole (a map of clusters) and learned a few topics at a time.

    communities  Louvain local moving over the concept graph (broader links
                 and co-occurrence weights), then fragments too small to be a
                 topic join the cluster they are most connected to. Pure and
                 deterministic.
    refresh!     clusters again after the graph changed (an ingest): concepts
                 in a cluster stay in it, new ones join a cluster or make new
                 ones, so knowledge the change does not touch keeps its
                 topics (or :fresh? true: cluster from scratch). A new cluster that is mostly an old
                 one keeps its id and name (so what the learner chose to
                 learn carries over); the others are named by the model. An
                 old cluster that is gone has a successor: the new cluster
                 holding most of its concepts.
    universe     the stored clusters for display: members, the source most of
                 their passages come from, links between clusters."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept.graph :as graph]
            [robertluo.fly-learner.concept.prereq :as prereq]
            [robertluo.fly-learner.llm :as llm]))

;;=============================================================================
;; Communities
;;=============================================================================

(defn- edge-weight [{:keys [kind weight]}]
  (if (= :broader kind) 1.0 (max 0.05 (double (or weight 0.1)))))

(defn- adjacency
  "{id {neighbour weight}} over `edges` ({:from :to :kind :weight})."
  [edges]
  (reduce (fn [m {:keys [from to] :as e}]
            (if (= from to)
              m
              (let [w (edge-weight e)]
                (-> m (update-in [from to] (fnil + 0.0) w) (update-in [to from] (fnil + 0.0) w)))))
          {} edges))

(defn- local-moving
  "One sweep of Louvain's first phase: each node (in a fixed order) moves to
  the neighbouring community that gains the most modularity."
  [adj deg m2 order comm]
  (let [tot (volatile! (reduce (fn [t [n c]] (update t c (fnil + 0.0) (deg n))) {} comm))]
    (reduce (fn [comm n]
              (let [c0    (comm n)
                    k     (deg n)
                    _     (vswap! tot update c0 - k)
                    links (reduce (fn [acc [nb w]] (update acc (comm nb) (fnil + 0.0) w)) {} (adj n))
                    gain  (fn [c] (- (get links c 0.0) (/ (* k (get @tot c 0.0)) m2)))
                    best  (reduce (fn [b c] (if (> (gain c) (+ (gain b) 1e-12)) c b))
                                  c0 (sort-by str (keys links)))]
                (vswap! tot update best (fnil + 0.0) k)
                (assoc comm n best)))
            comm order)))

(defn- absorb-small
  "Clusters under `min-size` join the cluster they share the most weight
  with (smallest first); unconnected ones stay, marked ::alone (and are not
  looked at again)."
  [adj groups min-size]
  (loop [groups (vec (sort-by (juxt count #(str (first (sort-by str %)))) groups))]
    (let [i (first (keep-indexed (fn [i g] (when (and (< (count g) min-size)
                                                      (not (::alone (meta g))) (not (::kept (meta g))))
                                             i))
                                 groups))
          g (when i (groups i))
          pull (when g
                 (->> (for [[j h] (map-indexed vector groups) :when (not= i j)]
                        [j (reduce + (for [n g [nb w] (adj n) :when (contains? h nb)] w))])
                      (filter (comp pos? second))
                      (sort-by (juxt (comp - second) first))
                      first))]
      (cond
        (nil? i)    groups
        (nil? pull) (recur (vec (sort-by (juxt count #(str (first (sort-by str %))))
                                         (assoc groups i (with-meta g {::alone true})))))
        :else       (let [j (first pull)]
                      (recur (vec (sort-by (juxt count #(str (first (sort-by str %))))
                                           (keep-indexed (fn [k h] (cond (= k j) (into h g) (= k i) nil :else h))
                                                         groups)))))))))

(defn communities
  "Topic clusters of the concepts `ids` linked by `edges`: [#{id ..} ..],
  largest first. Concepts without edges form no cluster. `:keep` {id key}:
  clusters there are (concepts with the same key are one): they stay as
  they are, and only the other concepts move, into them or into new
  clusters. So a graph that changed in one place keeps its clusters
  elsewhere; Louvain from scratch may settle anywhere, as the order it meets
  concepts in changes."
  ([ids edges] (communities ids edges {}))
  ([ids edges {:keys [sweeps min-size] stay :keep :or {sweeps 8 min-size 6}}]
   (let [in    (set ids)
         adj   (adjacency (filter #(and (in (:from %)) (in (:to %))) edges))
         order (sort-by str (keys adj))
         deg   (into {} (for [[n nb] adj] [n (reduce + (vals nb))]))
         m2    (reduce + (vals deg))]
     (if (zero? m2)
       []
       (let [start  (into {} (for [n order] [n (if-let [k (get stay n)] [:keep k] n)]))
             movers (remove #(contains? stay %) order)
             comm   (nth (iterate (partial local-moving adj deg m2 movers) start) sweeps)
             groups (map set (vals (group-by comm order)))
             kept?  (fn [g] (some #(contains? stay %) g))
             ;; clusters kept are kept, whatever their size
             [old new] [(filter kept? groups) (remove kept? groups)]]
         (->> (absorb-small adj (concat (map #(with-meta % {::kept true}) old) new) min-size)
              ;; a fragment connected to nothing is not a topic
              (remove #(and (::alone (meta %)) (< (count %) min-size)))
              (sort-by (juxt (comp - count) #(str (first (sort-by str %)))))
              vec))))))

^:rct/test
(comment
  ;; two triangles joined by one weak edge, and a pair hanging off the first
  (def es (concat (for [[a b] [[:a :b] [:b :c] [:a :c] [:x :y] [:y :z] [:x :z]]]
                    {:from a :to b :kind :related :weight 1.0})
                  [{:from :c :to :x :kind :related :weight 0.05}
                   {:from :p :to :a :kind :broader}]))
  (communities [:a :b :c :x :y :z :p] es {:min-size 2})
  ;=> [#{:a :b :c :p} #{:x :y :z}]
  ;; too small to stand alone: joins its neighbour
  (count (communities [:a :b :c :x :y :z :p] es {:min-size 4})) ;=> 1
  (communities [:a] []) ;=> []
  ;; a small part connected to nothing else is no topic (and does not stall)
  (communities [:a :b :c :x :y :z :p :q] (conj es {:from :p :to :q :kind :related :weight 1.0}
                                               {:from :u :to :v :kind :related :weight 1.0})
               {:min-size 3})
  ;=> [#{:a :b :c :p :q} #{:x :y :z}]
  (communities [:a :b :c :u :v] (conj es {:from :u :to :v :kind :related :weight 1.0}) {:min-size 3})
  ;=> [#{:a :b :c}]

  ;; keeping the clusters there are: a new concept joins one, the rest stay,
  ;; even where clustering from scratch would now group them otherwise
  (communities [:a :b :c :x :y :z :p :n] (conj es {:from :n :to :x :kind :related :weight 1.0})
               {:min-size 2 :keep {:a 1 :b 1 :c 1 :p 1 :x 2 :y 2 :z 2}})
  ;=> [#{:a :b :c :p} #{:x :y :z :n}]
  (communities [:a :b :c :x :y :z] es {:min-size 2 :keep {:a 1 :b 1 :c 2 :x 2 :y 2 :z 2}})
  ;=> [#{:c :x :y :z} #{:a :b}]
  )

;;=============================================================================
;; Identity across re-clustering
;;=============================================================================

(defn- jaccard [a b]
  (let [i (count (filter a b))]
    (if (zero? i) 0.0 (/ (double i) (- (+ (count a) (count b)) i)))))

(defn match
  "Pair new clusters `groups` (sets) with `old` ones ({:cluster/id :members
  set}): best overlap first, each old one used once, overlap (Jaccard) at
  least `min-overlap`. Returns {group-index old-id}."
  ([groups old] (match groups old 0.4))
  ([groups old min-overlap]
   (->> (for [[i g] (map-indexed vector groups)
              {:cluster/keys [id] :keys [members]} old
              :let [j (jaccard g members)]
              :when (>= j min-overlap)]
          [j i id])
        (sort-by (fn [[j i id]] [(- j) i (str id)]))
        (reduce (fn [[taken-i taken-id acc] [_ i id]]
                  (if (or (taken-i i) (taken-id id))
                    [taken-i taken-id acc]
                    [(conj taken-i i) (conj taken-id id) (assoc acc i id)]))
                [#{} #{} {}])
        last)))

^:rct/test
(comment
  (match [#{1 2 3 4} #{5 6 7}] [{:cluster/id :old-b :members #{5 6 8}}
                                {:cluster/id :old-a :members #{1 2 3}}])
  ;=> {0 :old-a 1 :old-b}
  ;; a cluster that is now mostly something else is not the same cluster
  (match [#{1 2 3 4 5 6 7 8}] [{:cluster/id :old :members #{1 9 10}}]) ;=> {}
  )

;;=============================================================================
;; Store
;;=============================================================================

(defn stored
  "Clusters in `db`: [{:cluster/id :cluster/label :cluster/gloss :members #{concept-id}}]."
  [db]
  (->> (d/q '[:find [(pull ?c [:cluster/id :cluster/label :cluster/gloss :cluster/concepts]) ...]
              :where [?c :cluster/id]]
            db)
       (mapv (fn [c] (-> c (assoc :members (set (:cluster/concepts c))) (dissoc :cluster/concepts))))))

(def ^:private namer-limit 15)

(defn- describe-group
  "What the namer sees of a cluster: its most mentioned concepts."
  [g group]
  (->> group
       (sort-by (fn [id] [(- (count (get-in g [:mentions id]))) (str id)]))
       (take namer-limit)
       (mapv (fn [id] (let [{:concept/keys [label gloss]} (get-in g [:concepts id])]
                        {:label label :gloss gloss})))))

(defn successors
  "For each of the `old` clusters ({:cluster/id :members}) not `kept` (a set
  of ids), the new cluster holding most of its members: {old-id new-id}.
  `new` is [{:cluster/id :members}]; an old cluster none of whose members
  is in a new one has none."
  [old kept new]
  (into {} (for [{:cluster/keys [id] :keys [members]} old
                 :when (not (kept id))
                 :let [[best n] (->> new
                                     (map (fn [c] [(:cluster/id c) (count (filter (:members c) members))]))
                                     (sort-by (fn [[cid n]] [(- n) (str cid)]))
                                     first)]
                 :when (and best (pos? n))]
             [id best])))

^:rct/test
(comment
  (successors [{:cluster/id :a :members #{1 2 3}} {:cluster/id :b :members #{4}} {:cluster/id :c :members #{9}}]
              #{:b}
              [{:cluster/id :x :members #{1 4}} {:cluster/id :y :members #{2 3 5}}])
  ;=> {:a :y}
  )

(defn refresh!
  "Cluster the concept graph in `conn` again and store the clusters: the
  stored ones stay, new concepts join them or make new ones (see
  `communities` :keep), unless `:fresh?`. A new cluster
  that is mostly an old one keeps its id and name; `namer` names the rest:
  it gets {:clusters [[{:label :gloss} ..] ..] :named [label ..]} and
  returns {:names [{:label :gloss} ..] (one per cluster, in order) :by who}.
  Returns {:clusters n :kept n :named n :dropped n :successors {old-id
  new-id}} (see `successors`: for what referred to a dropped cluster)."
  ([conn namer] (refresh! conn namer {}))
  ([conn namer {:keys [fresh?]}]
   (let [db     (d/db conn)
         g      (graph/load-graph db)
         edges  (prereq/edge-list db)
         old    (stored db)
         home   (into {} (for [{:cluster/keys [id] :keys [members]} old
                               c members :when (get-in g [:concepts c])]
                           [c id]))
         groups (communities (keys (:concepts g)) edges (if fresh? {} {:keep home}))
         ;; kept clusters are themselves, however much they grew; from scratch,
         ;; a cluster that is mostly an old one is that one
         kept   (if fresh?
                  (match groups old)
                  (into {} (keep-indexed (fn [i grp] (some->> (some home grp) (vector i)))) groups))
         fresh  (vec (remove kept (range (count groups))))
         names  (when (seq fresh)
                  (namer {:clusters (mapv #(describe-group g (groups %)) fresh)
                          :named    (vec (keep #(:cluster/label (some (fn [o] (when (= (kept %) (:cluster/id o)) o)) old))
                                               (keys kept)))}))
         by     (:by names "unknown")
         named  (zipmap fresh (:names names))
         now    (java.util.Date.)
         tx     (vec (for [[i grp] (map-indexed vector groups)]
                       (if-let [id (kept i)]
                         {:cluster/id id :cluster/concepts (vec (sort-by str grp)) :cluster/at now}
                         (let [{:keys [label gloss]} (named i)]
                           (cond-> {:cluster/id       (random-uuid)
                                    :cluster/label    (or (not-empty label)
                                                          (str/join " · " (map :label (take 2 (describe-group g grp)))))
                                    :cluster/concepts (vec (sort-by str grp))
                                    :cluster/by       by
                                    :cluster/at       now}
                             (seq gloss) (assoc :cluster/gloss gloss))))))
         gone   (remove (set (vals kept)) (map :cluster/id old))]
     (d/transact! conn (into tx (for [id gone] [:db/retractEntity [:cluster/id id]])))
     {:clusters   (count groups) :kept (count kept) :named (count fresh) :dropped (count gone)
      :successors (successors old (set (vals kept))
                              (for [e tx] {:cluster/id (:cluster/id e) :members (set (:cluster/concepts e))}))})))

;;=============================================================================
;; The model names clusters
;;=============================================================================

(def ^:private namer-system
  "You name the topics of a personal knowledge base. Its concepts were grouped
into clusters by how they connect (shared passages, is-a links). For each
cluster you get its most mentioned concepts with their glosses.

Give each cluster:
- label: the topic in 2 to 5 words, as a textbook chapter would name it
  (\"Bayesian games\", \"Confluence of reduction\"). Not a list of concepts.
- gloss: one sentence on what the cluster covers, for a learner deciding
  whether to study it.

Labels must be distinct from each other and from the topics already named.")

(def ^:private namer-schema
  {"type"                 "object"
   "properties"           {"names" {"type"  "array"
                                    "items" {"type"                 "object"
                                             "properties"           {"label" {"type" "string"}
                                                                     "gloss" {"type" "string"}}
                                             "required"             ["label" "gloss"]
                                             "additionalProperties" false}}}
   "required"             ["names"]
   "additionalProperties" false})

(defn- namer-prompt [{:keys [clusters named]}]
  (str (when (seq named)
         (str "Topics already named: " (str/join "; " named) "\n\n"))
       "Name these " (count clusters) " clusters, in order:\n\n"
       (str/join "\n\n"
                 (map-indexed (fn [i cs]
                                (str "Cluster " (inc i) ":\n"
                                     (str/join "\n" (for [{:keys [label gloss]} cs]
                                                      (str "- " label (when gloss (str ": " gloss)))))))
                              clusters))))

(defn llm-namer
  "Namer backed by Claude (the :strong tier). opts: :models tier overrides."
  [client {:keys [models]}]
  (let [model (llm/model-for models :strong)]
    (fn [batch]
      (let [{:keys [names]} (llm/complete-json client {:system     namer-system
                                                       :prompt     (namer-prompt batch)
                                                       :model      model
                                                       :max-tokens 4000
                                                       :schema     namer-schema})]
        {:names (vec names) :by model}))))

;;=============================================================================
;; Reading
;;=============================================================================

(defn universe
  "Every cluster for display, largest first:
  {:clusters [{:cluster/id :cluster/label :cluster/gloss :concepts [id ..]
               :source {:source/id :source/title}} ..]
   :links [{:from cluster-id :to cluster-id :weight w} ..]}
  :concepts keeps live concepts only (most mentioned first); :source is where
  most of the cluster's passages come from; :weight sums the edges between
  two clusters."
  [db]
  (let [g       (graph/load-graph db)
        edges  (prereq/edge-list db)
        src-of  (into {} (map (fn [[n sid st]] [n {:source/id sid :source/title st}]))
                      (d/q '[:find ?nid ?sid ?st
                             :where [?n :node/id ?nid] [?n :node/source ?s]
                             [?s :source/id ?sid] [?s :source/title ?st]]
                           db))
        cs      (->> (stored db)
                     (map (fn [{:keys [members] :as c}]
                            (let [live (filter #(get-in g [:concepts %]) members)]
                              (assoc (dissoc c :members)
                                     :concepts (vec (sort-by (fn [id] [(- (count (get-in g [:mentions id]))) (str id)]) live))
                                     :source (->> live
                                                  (mapcat #(get-in g [:mentions %]))
                                                  (keep src-of)
                                                  frequencies
                                                  (sort-by (comp - val))
                                                  ffirst)))))
                     (filter (comp seq :concepts))
                     (sort-by (comp - count :concepts))
                     vec)
        home    (into {} (for [c cs id (:concepts c)] [id (:cluster/id c)]))
        links   (->> edges
                     (keep (fn [{:keys [from to] :as e}]
                             (let [a (home from) b (home to)]
                               (when (and a b (not= a b))
                                 [(sort-by str [a b]) (edge-weight e)]))))
                     (reduce (fn [m [k w]] (update m k (fnil + 0.0) w)) {})
                     (mapv (fn [[[a b] w]] {:from a :to b :weight w})))]
    {:clusters cs :links links}))
