(ns robertluo.fly-learner.ui.core.graph
  "The concept graph explored by expanding and collapsing concepts.

  Data: the whole graph from the API (robertluo.fly-learner.concept.view/graph),
  indexed once by `index`. What is shown is derived, like an outline:

    visible = the focus, plus the neighbours of every expanded concept
              that is itself visible

  so collapsing a concept hides what only it revealed, and expanding it
  again brings back the subtree as it was. A concept's neighbours: its
  broader and narrower concepts, then its strongest related ones.

  Positions: robertluo.fly-learner.ui.core.layout.")

(def related-per-concept
  "Related concepts revealed by expanding one (strongest first)."
  6)

(defn index
  "Index the API's {:concepts .. :edges ..}:
  {:concepts {id ref} :nbrs {id [id ..]} :edges [edge ..] :broader {id #{parent}}}."
  ([data] (index data related-per-concept))
  ([{:keys [concepts edges]} k]
   (let [broader  (filter #(= :broader (:kind %)) edges)
         related  (filter #(= :related (:kind %)) edges)
         parents  (reduce (fn [m {:keys [from to]}] (update m from (fnil conj #{}) to)) {} broader)
         children (reduce (fn [m {:keys [from to]}] (update m to (fnil conj #{}) from)) {} broader)
         strongest (->> related
                        (mapcat (fn [{:keys [from to weight]}] [[from [weight to]] [to [weight from]]]))
                        (reduce (fn [m [a wb]] (update m a (fnil conj []) wb)) {})
                        (into {} (map (fn [[a wbs]]
                                        [a (->> wbs (sort-by (comp - first)) (map second))]))))
         label    (into {} (map (juxt :concept/id :concept/label)) concepts)
         by-label #(sort-by label %)]
     {:concepts (into {} (map (juxt :concept/id identity)) concepts)
      :broader  parents
      :edges    (vec edges)
      :nbrs     (into {}
                      (for [{:concept/keys [id]} concepts]
                        (let [hier (concat (by-label (parents id)) (by-label (children id)))]
                          [id (vec (distinct (concat hier
                                                     (take k (remove (set hier) (strongest id))))))])))})))

(defn neighbours [ix id] (get-in ix [:nbrs id] []))

(defn visible
  "Concepts shown for `focus` with `expanded` concepts open."
  [ix focus expanded]
  (if-not (get-in ix [:concepts focus])
    #{}
    (loop [seen #{focus} queue [focus]]
      (if-let [[id & more] (seq queue)]
        (if (contains? expanded id)
          (let [new (remove seen (neighbours ix id))]
            (recur (into seen new) (into (vec more) new)))
          (recur seen more))
        seen))))

(defn revealer
  "The visible expanded concept whose expansion shows `id` (first found), or nil."
  [ix vis expanded id]
  (some (fn [e] (when (and (vis e) (some #{id} (neighbours ix e))) e)) (sort-by str expanded)))

(defn edges
  "Edges among `vis`: every broader link, and related links where one end
  lists the other among its neighbours (so not every weak co-occurrence)."
  [ix vis]
  (filterv (fn [{:keys [from to kind]}]
             (and (vis from) (vis to)
                  (or (= :broader kind)
                      (some #{to} (neighbours ix from))
                      (some #{from} (neighbours ix to)))))
           (:edges ix)))

(defn hidden-count
  "Neighbours of `id` not shown: what expanding it would add."
  [ix vis id]
  (count (remove vis (neighbours ix id))))

^:rct/test
(comment
  ;; :t is a kind of :p; :t relates to :a (strong), :b, :c (weak); :a relates to :d
  (def data {:concepts [{:concept/id :p :concept/label "p"} {:concept/id :t :concept/label "t"}
                        {:concept/id :a :concept/label "a"} {:concept/id :b :concept/label "b"}
                        {:concept/id :c :concept/label "c"} {:concept/id :d :concept/label "d"}]
             :edges [{:from :t :to :p :kind :broader}
                     {:from :a :to :t :kind :related :weight 0.9}
                     {:from :b :to :t :kind :related :weight 0.5}
                     {:from :c :to :t :kind :related :weight 0.1}
                     {:from :a :to :d :kind :related :weight 0.4}
                     {:from :b :to :c :kind :related :weight 0.3}]})
  (def ix (index data 2))

  ;; hierarchy first, then the 2 strongest related
  (neighbours ix :t) ;=> [:p :a :b]

  (visible ix :t #{}) ;=> #{:t}
  (visible ix :t #{:t}) ;=> #{:t :p :a :b}
  (visible ix :t #{:t :a}) ;=> #{:t :p :a :b :d}
  ;; collapsing :t hides :a's subtree too; :a stays expanded for next time
  (visible ix :t #{:a}) ;=> #{:t}
  (visible ix :nope #{}) ;=> #{}

  (revealer ix #{:t :p :a :b :d} #{:t :a} :d) ;=> :a

  ;; :b-:c is not shown while :c is hidden; :a-:d once both are
  (set (map (juxt :from :to) (edges ix (visible ix :t #{:t :a}))))
  ;=> #{[:t :p] [:a :t] [:b :t] [:a :d]}

  (hidden-count ix #{:t} :t) ;=> 3
  (hidden-count ix (visible ix :t #{:t}) :t) ;=> 0
  )
