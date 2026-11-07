(ns robertluo.fly-learner.ideas.knowledge
  "Anchors over the knowledge, as robertluo.fly-learner.ideas/Anchors: the
  one place where ideas meet knowledge.

  Anchors (what an idea is about), and the target key each resolves to:
    {:anchor/kind :concept :concept/id id}               -> [:concept id]
    {:anchor/kind :relation :concept/ids [a b]}          -> [:relation a b]  (a < b as strings)
    {:anchor/kind :node :node/id id :source/id s :node/title t}  -> [:node id]
    {:anchor/kind :quote :source/id s :field f :node/id hint :page p
     :quote .. :prefix .. :suffix ..}                    -> [:node id] or [:page s p]
      f is :text or :summary of a node, or :pdf (selected on the page)

  Knowledge changes under ideas; anchors are re-attached rather than lost:
  a merged concept is followed to the concept it was merged into (concept
  edit log); a node gone after re-ingest is found again by title in its
  source; a quote is found again in its node, then anywhere in its source
  (whitespace may differ); a quote from the PDF that no paragraph contains
  stays on its page."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.ideas :as ideas]))

(defn relation-key [a b]
  (let [[a b] (sort-by str [a b])] [:relation a b]))

(defn- merges
  "merged concept id -> the concept it was merged into, from the edit log."
  [db]
  (into {} (for [{:edit/keys [op args]} (concept/edits db) :when (= :merge op)]
             [(second args) (first args)])))

(defn- live-concept [db merged id]
  (loop [id id, n 0]
    (cond
      (d/entid db [:concept/id id]) id
      (and (merged id) (< n 20))   (recur (merged id) (inc n)))))

(defn- source-nodes
  "[[node-id title text summary page] ..] of the source `sid`."
  [db sid]
  (d/q '[:find ?id ?t ?x ?s ?p
         :in $ ?sid
         :where [?src :source/id ?sid] [?n :node/source ?src] [?n :node/id ?id]
         [(get-else $ ?n :node/title "") ?t] [(get-else $ ?n :node/text "") ?x]
         [(get-else $ ?n :node/summary "") ?s] [(get-else $ ?n :node/page 0) ?p]]
       db sid))

(defn- field-text [[_ _ text summary] field]
  (if (= :summary field) summary text))

(defn anchors
  "Anchors over the knowledge in `db` (a snapshot)."
  [db]
  (let [merged (delay (merges db))
        nodes  (memoize #(source-nodes db %))
        concept-id #(live-concept db @merged %)]
    (reify ideas/Anchors
      (target [_ {:anchor/keys [kind] :as a}]
        (case kind
          :concept
          (some->> (:concept/id a) concept-id (vector :concept))

          :relation
          (let [[x y] (map concept-id (:concept/ids a))]
            (when (and x y (not= x y)) (relation-key x y)))

          :node
          (let [{:node/keys [id title] sid :source/id} a]
            (if (d/entid db [:node/id id])
              [:node id]
              (some (fn [[nid t]] (when (= t title) [:node nid])) (nodes sid))))

          :quote
          (let [{:keys [field page] nid :node/id sid :source/id} a
                in?  (fn [text] (ideas/find-quote text a))
                hint (when (and nid (not= :pdf field)) (some #(when (= nid (first %)) %) (nodes sid)))]
            (cond
              (and hint (in? (field-text hint field))) [:node nid]
              (= :pdf field)
              (or (some (fn [[id _ text _ p]]
                          (when (and (<= (abs (- p page)) 1) (not (str/blank? text)) (in? text)) [:node id]))
                        (nodes sid))
                  [:page sid page])
              :else
              (some (fn [n] (when (in? (field-text n field)) [:node (first n)])) (nodes sid))))

          nil))

      (describe [_ [kind a b]]
        (case kind
          :concept  {:kind :concept :title (:concept/label (d/pull db [:concept/label] [:concept/id a]))}
          :relation {:kind :relation
                     :title (str (:concept/label (d/pull db [:concept/label] [:concept/id a])) " — "
                                 (:concept/label (d/pull db [:concept/label] [:concept/id b])))}
          :node     (let [{:node/keys [title kind page source]}
                          (d/pull db [:node/title :node/kind :node/page {:node/source [:source/id :source/title]}]
                                  [:node/id a])]
                      {:kind :node :title title :node/kind kind :page page
                       :source/id (:source/id source) :source/title (:source/title source)})
          :page     {:kind :page
                     :title (str (:source/title (d/pull db [:source/title] [:source/id a])) ", p. " b)}
          {:kind kind :title (str kind)})))))
