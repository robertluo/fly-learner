(ns robertluo.fly-learner.concept.domain
  "Domains: the regions of the map of topics, named, as a world map names its
  regions. A domain is a set of topics that hang together more than with the
  rest: its core (Louvain over the links between topics, cluster/communities),
  and every topic from elsewhere tied to it nearly as strongly as to anything
  else. So domains do not own topics: a topic between two fields is in both,
  and domains overlap.

    groups   pure: the domains of a universe ({:clusters :links}), as sets of
             topic ids, largest first
    named    the groups with their names: a group that is mostly a domain
             named before keeps its id and name (cluster/match), the others
             have no name yet
    name!    names the groups without one (the model) and stores them
  Stored as :domain/* (db schema)."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept.cluster :as cluster]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.prompts :as prompts]))

;;=============================================================================
;; Groups
;;=============================================================================

(def ^:private overlap-share
  "A topic is in a domain other than its own when its ties to it are at least
  this share of its ties to the domain it is most tied to."
  0.5)

(defn groups
  "The domains of topics `ids` linked by `links` ({:from :to :weight}):
  [{:core #{id ..} :topics #{id ..}} ..], largest core first. A core holds at least `min-size` topics; a
  topic is in each domain it is tied to at least `overlap-share` as strongly
  as to the one it is most tied to (its own core counts what ties it to the
  rest of the core)."
  ([ids links] (groups ids links {}))
  ([ids links {:keys [min-size share] :or {min-size 3 share overlap-share}}]
   (let [in    (set ids)
         links (filter #(and (in (:from %)) (in (:to %)) (not= (:from %) (:to %))) links)
         cores (cluster/communities ids (map #(assoc % :kind :related) links) {:min-size min-size})
         adj   (reduce (fn [m {:keys [from to weight]}]
                         (-> m (update-in [from to] (fnil + 0.0) weight) (update-in [to from] (fnil + 0.0) weight)))
                       {} links)
         tie   (fn [t core] (reduce + 0.0 (for [[nb w] (adj t) :when (contains? core nb)] w)))]
     (vec (for [core cores]
            {:core   core
             :topics (into core (for [t (sort-by str in)
                             :when (not (contains? core t))
                             :let [ties (map #(tie t %) cores)
                                   best (reduce max 0.0 ties)
                                   here (tie t core)]
                             :when (and (pos? here) (>= here (* share best)))]
                         t))})))))

^:rct/test
(comment
  ;; two triangles; :m between them belongs to both, :e to the first alone
  (def ls (concat (for [[a b] [[:a :b] [:b :c] [:a :c] [:x :y] [:y :z] [:x :z]]]
                    {:from a :to b :weight 4.0})
                  [{:from :m :to :a :weight 3.0} {:from :m :to :x :weight 2.0}
                   {:from :e :to :b :weight 3.0} {:from :e :to :y :weight 0.5}]))
  (set (map :topics (groups [:a :b :c :x :y :z :m :e] ls)))
  ;=> #{#{:a :b :c :m :e} #{:x :y :z :m}}
  (set (map :core (groups [:a :b :c :x :y :z :m :e] ls)))
  ;=> #{#{:a :b :c :e :m} #{:x :y :z}}
  (groups [:a] []) ;=> []
  )

;;=============================================================================
;; Names
;;=============================================================================

(defn stored
  "Domains named in `db`: [{:domain/id :domain/label :domain/gloss :topics #{cluster-id}}]."
  [db]
  (->> (d/q '[:find [(pull ?d [:domain/id :domain/label :domain/gloss :domain/topics]) ...]
              :where [?d :domain/id]]
            db)
       (mapv (fn [m] (-> m (assoc :topics (set (:domain/topics m))) (dissoc :domain/topics))))))

(defn named
  "The domains of `universe` ({:clusters :links}, cluster/universe), largest
  first: [{:domain/id :domain/label :domain/gloss :topics [cluster-id ..]
  :core [cluster-id ..]}]. A group that is mostly a domain named before is
  that one (cluster/match); the others have no :domain/id nor label yet."
  [db {:keys [clusters links]}]
  (let [gs     (groups (map :cluster/id clusters) links)
        old    (stored db)
        by-id  (into {} (map (juxt :domain/id identity)) old)
        hits   (cluster/match (map :topics gs) (map (fn [{:domain/keys [id] :keys [topics]}] {:cluster/id id :members topics}) old))]
    (vec (for [[i {:keys [core topics]}] (map-indexed vector gs)
               :let [d (by-id (hits i))]]
           (merge (select-keys d [:domain/id :domain/label :domain/gloss])
                  {:topics (vec (sort-by str topics)) :core (vec (sort-by str core))})))))

(def ^:private namer-limit 12)

(def ^:private namer-system
  (prompts/prompt!
   :domains/name
   {:group "Concepts" :title "Name the domains"
    :about "The map's regions: a name for each group of topics that sit together (strong tier). The reply is JSON."
    :default
    "You name the regions of a map of knowledge, as a world map names its
regions. Its topics were grouped by how they connect; a topic may be in more
than one region. For each region you get its topics with what they cover,
the most central first.

Give each region:
- label: the field in 1 to 3 words, as a library shelf or a university
  department would name it (\"Game theory\", \"Type theory\", \"Interpreters
  and compilers\"). Broader than any one of its topics; not a list.
- gloss: one sentence on what the region covers.

Labels must be distinct from each other and from the regions already named."}))

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

(defn- namer-prompt [{:keys [domains named]}]
  (str (when (seq named)
         (str "Regions already named: " (str/join "; " named) "\n\n"))
       "Name these " (count domains) " regions, in order:\n\n"
       (str/join "\n\n"
                 (map-indexed (fn [i ts]
                                (str "Region " (inc i) ":\n"
                                     (str/join "\n" (for [{:keys [label gloss]} ts]
                                                      (str "- " label (when gloss (str ": " gloss)))))))
                              domains))))

(defn llm-namer
  "Namer backed by the :strong model: {:domains [[{:label :gloss} ..] ..]
  :named [label ..]} -> {:names [{:label :gloss} ..] :by model}."
  [client {:keys [models]}]
  (let [model (llm/model-for models :strong)]
    (fn [batch]
      (let [{:keys [names]} (llm/complete-json client {:system     (prompts/text namer-system)
                                                       :prompt     (namer-prompt batch)
                                                       :model      model
                                                       :max-tokens 3000
                                                       :schema     namer-schema})]
        {:names (vec names) :by model}))))

(defn name!
  "Name the domains of `universe` that have no name yet (`namer`, see
  llm-namer) and store them; stored domains no group is any more go.
  Returns the number named."
  [conn universe namer]
  (let [db      (d/db conn)
        ds      (named db universe)
        fresh   (filterv (complement :domain/id) ds)
        topic   (into {} (map (juxt :cluster/id identity)) (:clusters universe))
        ;; what the namer sees: the core first (largest topics first), then the rest
        seen    (fn [{:keys [topics core]}]
                  (let [in-core (set core)
                        size    #(count (:concepts (topic %)))]
                    (->> topics
                         (sort-by (fn [id] [(if (in-core id) 0 1) (- (size id)) (str id)]))
                         (take namer-limit)
                         (mapv #(select-keys (topic %) [:cluster/label :cluster/gloss]))
                         (mapv (fn [{:cluster/keys [label gloss]}] {:label label :gloss gloss})))))
        kept    (set (keep :domain/id ds))
        gone    (remove kept (map :domain/id (stored db)))]
    (when (seq fresh)
      (let [{:keys [names by]} (namer {:domains (mapv seen fresh)
                                       :named   (vec (keep :domain/label ds))})
            now (java.util.Date.)]
        (d/transact! conn (vec (concat
                                (for [[{:keys [topics]} {:keys [label gloss]}] (map vector fresh names)
                                      :when (not (str/blank? label))]
                                  (cond-> {:domain/id (random-uuid) :domain/label label
                                           :domain/topics (set topics) :domain/by (or by "unknown") :domain/at now}
                                    (not (str/blank? gloss)) (assoc :domain/gloss gloss)))
                                (for [id gone] [:db/retractEntity [:domain/id id]]))))))
    (when (and (empty? fresh) (seq gone))
      (d/transact! conn (vec (for [id gone] [:db/retractEntity [:domain/id id]]))))
    (count fresh)))
