(ns robertluo.fly-learner.concept.graph
  "The concept graph and operations over it.

  `load-graph` reads {:concepts {id concept} :mentions {id #{node-id}}} from
  the db; everything else is pure over that value, so it is cheap to rebuild
  after any edit (a few hundred concepts per source fit in memory easily).

  Edges are derived, never stored:
    :broader / :narrower  from :concept/broader
    :related              co-occurrence: both concepts mentioned by the same
                          nodes, scored by normalised PMI (so a concept
                          mentioned everywhere, like a document's subject,
                          is not related to everything). The shared nodes
                          are the edge's evidence.

  Operations: `neighbourhood` (k-hop subgraph), `path` (cheapest connection
  between two concepts, each step with its evidence), `related` (strongest
  neighbours)."
  (:require [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.db :as db]))

(def load-graph
  "{:concepts {id concept} :mentions {id #{node-id}}} of a db, read once per
  db version (db/per-version): most pages need it, often at once."
  (db/per-version
   (fn [db]
     {:concepts (into {} (map (juxt :concept/id identity)) (concept/concepts db))
      :mentions (reduce (fn [m [c n]] (update m c (fnil conj #{}) n))
                        {}
                        (d/q '[:find ?cid ?nid
                               :where [?c :concept/aliases ?a] [?n :node/keywords ?a]
                               [?c :concept/id ?cid] [?n :node/id ?nid]]
                             db))})))

;;=============================================================================
;; Edges
;;=============================================================================

(defn cooccurrence
  "{#{a b} {:count n :npmi x :nodes #{node ..}}} for concept pairs mentioned
  together by at least `min-count` nodes with NPMI above `min-npmi`.
  NPMI is in [-1, 1]: 1 when the two are only ever mentioned together, 0 when
  they co-occur as often as chance predicts."
  ([g] (cooccurrence g {}))
  ([{:keys [mentions]} {:keys [min-count min-npmi] :or {min-count 1 min-npmi 0.0}}]
   (let [by-node (reduce-kv (fn [m c ns] (reduce #(update %1 %2 (fnil conj #{}) c) m ns)) {} mentions)
         total   (double (count by-node))
         pairs   (reduce-kv (fn [m n cs]
                              (reduce #(update %1 %2 (fnil conj #{}) n) m
                                      (for [a cs, b cs :when (neg? (compare a b))] #{a b})))
                            {} by-node)]
     (into {}
           (keep (fn [[pair ns]]
                   (let [[a b] (seq pair)
                         n-ab  (count ns)
                         p-ab  (/ n-ab total)
                         npmi  (if (= 1.0 p-ab)
                                 1.0
                                 (/ (Math/log (/ p-ab (* (/ (count (mentions a)) total)
                                                         (/ (count (mentions b)) total))))
                                    (- (Math/log p-ab))))]
                     (when (and (>= n-ab min-count) (> npmi min-npmi))
                       [pair {:count n-ab :npmi npmi :nodes ns}]))))
           pairs))))

(def broader-weight 0.8)

(defn- related-weight
  "NPMI damped by thin evidence: one shared node counts half, three 3/4."
  [{:keys [count npmi]}]
  (* npmi (/ count (inc count))))

(defn edges
  "Undirected adjacency {a {b edge}}, edge {:kind :broader|:narrower|:related
  :weight (0, 1] :nodes evidence}. :broader means b is a's broader concept.
  A broader link wins over co-occurrence between the same pair.
  opts go to `cooccurrence`."
  ([g] (edges g {}))
  ([{:keys [concepts] :as g} opts]
   (let [add   (fn [m a b e] (assoc-in m [a b] e))
         rel   (reduce-kv (fn [m pair e]
                            (let [[a b] (seq pair), e {:kind :related :weight (related-weight e)
                                                       :npmi (:npmi e) :nodes (:nodes e)}]
                              (-> m (add a b e) (add b a e))))
                          {} (cooccurrence g opts))]
     (reduce-kv (fn [m id {:concept/keys [broader]}]
                  (reduce (fn [m p]
                            (if (concepts p)
                              (-> m
                                  (add id p {:kind :broader :weight broader-weight})
                                  (add p id {:kind :narrower :weight broader-weight}))
                              m))
                          m broader))
                rel concepts))))

;;=============================================================================
;; Operations
;;=============================================================================

(defn neighbourhood
  "Concepts within `hops` of `id` and the edges among them:
  {:concepts #{id ..} :edges [[a b edge] ..]}, each undirected edge once
  (its :kind read from a: :broader means b is a's parent)."
  [adj id hops]
  (let [ids (loop [seen #{id} frontier #{id} k hops]
              (if (or (zero? k) (empty? frontier))
                seen
                (let [next (into #{} (comp (mapcat (comp keys adj)) (remove seen)) frontier)]
                  (recur (into seen next) next (dec k)))))]
    {:concepts ids
     :edges    (vec (for [a ids, [b e] (adj a)
                          :when (and (ids b) (neg? (compare a b)))]
                      [a b e]))}))

(def hop-cost
  "Added per step, so of two equally strong routes the shorter wins."
  0.2)

(defn cost [{:keys [weight]}] (+ hop-cost (- (Math/log weight))))

(defn path
  "Cheapest route from concept `from` to `to` (Dijkstra, step cost
  hop-cost - ln weight), as [{:concept from} {:concept c :via edge} ..
  {:concept to :via edge}], or nil when they are not connected."
  [adj from to]
  (loop [dist  {from 0.0}
         prev  {}
         queue (sorted-set [0.0 from])
         done  #{}]
    (if-let [[du u :as head] (first queue)]
      (cond
        (= u to)
        (loop [c to, out ()]
          (if (= c from)
            (vec (cons {:concept from} out))
            (let [[p e] (prev c)] (recur p (cons {:concept c :via e} out)))))

        (done u) (recur dist prev (disj queue head) done)

        :else
        (let [[dist prev queue]
              (reduce-kv (fn [[dist prev queue :as acc] v e]
                           (let [dv (+ du (cost e))]
                             (if (< dv (dist v Double/POSITIVE_INFINITY))
                               [(assoc dist v dv) (assoc prev v [u e]) (conj queue [dv v])]
                               acc)))
                         [dist prev (disj queue head)]
                         (adj u))]
          (recur dist prev queue (conj done u))))
      nil)))

(defn related
  "The `n` strongest neighbours of `id`: [[concept-id edge] ..]."
  [adj id n]
  (->> (adj id) (sort-by (comp - :weight val)) (take n) vec))

^:rct/test
(comment
  ;; nodes :n1..:n4; :a and :b always together, :c with :b once, :d alone
  (def g {:concepts {:a {:concept/id :a} :b {:concept/id :b} :c {:concept/id :c}
                     :d {:concept/id :d :concept/broader #{:a}}}
          :mentions {:a #{:n1 :n2} :b #{:n1 :n2 :n3} :c #{:n3 :n4} :d #{:n4}}})

  (update-vals (cooccurrence g) #(select-keys % [:count :nodes]))
  ;=> {#{:a :b} {:count 2 :nodes #{:n1 :n2}}
  ;    #{:c :d} {:count 1 :nodes #{:n4}}}
  ;; (:b :c share :n3, but :b is in 3 of 4 nodes: no more than chance)

  (def adj (edges g))
  (get-in adj [:d :a :kind]) ;=> :broader
  (get-in adj [:a :d :kind]) ;=> :narrower

  (mapv :concept (path adj :b :c)) ;=> [:b :a :d :c]
  (path adj :a :nowhere) ;=> nil

  (:concepts (neighbourhood adj :a 1)) ;=> #{:a :b :d}
  (count (:edges (neighbourhood adj :a 1))) ;=> 2
  )
