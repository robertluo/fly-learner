(ns robertluo.fly-learner.concept.prereq
  "Prerequisites: concept a is best grasped before concept b.

  The concept graph's other edges say nothing of order: :broader is is-a /
  part-of, co-occurrence is symmetric. A prerequisite is directed: b's
  definition builds on a, or b cannot be explained without a. Concepts with
  no prerequisite path between them are independent: a learner takes them
  in any order.

  Hidden concepts: background the sources take for granted (\"probability\"
  in a game theory book) is in no keyword, so no concept names it. When
  judging a concept the model may name such background; names that mean the
  same are merged into one concept with :concept/origin :inferred and no
  mentions. Several concepts sharing one is the common ground to learn
  first. Its label is an alias, so a later source that does teach it gives
  it mentions like any other concept.

    judge       the model reads concepts (gloss, where the sources introduce
                them, their first passages, what they are discussed with)
                against the whole concept index, and names for each the
                concepts it needs and the background it assumes
    consolidate merges the background names of a judgement into hidden
                concepts (or finds they are concepts after all)
    ->tx        a judgement as tx data: :prereq/* claims, hidden concepts,
                each judged concept marked :concept/prereqs-by
    dag         the claims as an acyclic graph: mutual claims drop out (the
                two are learned together), a cycle loses the edge the
                sources' order contradicts most
    before      what to learn before a concept, in an order that works

  Claims are stored as the model made them (:prereq/*, schema in
  robertluo.fly-learner.db); `dag` is derived on read."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept.graph :as graph]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.llm :as llm]))

(defn- norm [s] (-> s str/trim str/lower-case (str/replace #"\s+" " ")))

;;=============================================================================
;; Reading
;;=============================================================================

(defn claims
  "Every stored claim: [{:of concept-id :needs concept-id :why :by}]."
  [db]
  (->> (d/q '[:find ?b ?a ?why ?by
              :where [?p :prereq/of ?be] [?p :prereq/needs ?ae] [?p :prereq/why ?why] [?p :prereq/by ?by]
              [?be :concept/id ?b] [?ae :concept/id ?a]]
            db)
       (mapv (fn [[b a why by]] {:of b :needs a :why why :by by}))))

(defn first-pages
  "{concept-id {source-id first-page}}: where each source first mentions it."
  [db]
  (->> (d/q '[:find ?cid ?sid (min ?p)
              :where [?c :concept/aliases ?a] [?n :node/keywords ?a] [?n :node/page ?p]
              [?n :node/source ?s] [?s :source/id ?sid] [?c :concept/id ?cid]]
            db)
       (reduce (fn [m [c s p]] (assoc-in m [c s] p)) {})))

;;=============================================================================
;; The DAG
;;=============================================================================

(defn- contradiction
  "How much the sources' order contradicts `needs` before `of`: the most
  pages `needs` is introduced after `of` in a source both are in (0 when
  none, or no source has them the wrong way round)."
  [pages {:keys [of needs]}]
  (let [po (pages of) pn (pages needs)]
    (reduce max 0 (for [[s p] po :let [q (get pn s)] :when q] (- q p)))))

(defn- find-cycle
  "Edges [of needs] of one cycle in `adj` {of #{needs}}, or nil."
  [adj]
  (let [state (volatile! {})]                       ; node -> :open | :done
    (letfn [(visit [path u]
              (vswap! state assoc u :open)
              (or (some (fn [v]
                          (case (@state v)
                            :open (let [cyc (conj (vec (drop-while #(not= v %) path)) u v)]
                                    (mapv vector cyc (rest cyc)))
                            :done nil
                            (visit (conj path u) v)))
                        (sort-by str (adj u)))
                  (do (vswap! state assoc u :done) nil)))]
      (some #(when-not (@state %) (visit [] %)) (sort-by str (keys adj))))))

(defn dag
  "The claims as an acyclic graph {of #{needs}}. Two concepts claiming each
  other are learned together: both claims drop. Of a longer cycle, the edge
  the sources' order contradicts most goes (see `contradiction`), until no
  cycle is left. `pages` is `first-pages`."
  [claims pages]
  (let [pairs  (set (map (juxt :of :needs) claims))
        kept   (remove (fn [{:keys [of needs]}] (or (= of needs) (pairs [needs of]))) claims)
        weight (into {} (map (fn [c] [[(:of c) (:needs c)] (contradiction pages c)])) kept)]
    (loop [adj (reduce (fn [m {:keys [of needs]}] (update m of (fnil conj #{}) needs)) {} kept)]
      (if-let [cyc (find-cycle adj)]
        (let [[of needs] (apply max-key #(weight % 0) (reverse cyc))]
          (recur (update adj of disj needs)))
        adj))))

(defn before
  "What to learn before concept `id`, every concept it needs directly or
  through others, each after the ones it needs (a topological order; ties
  by label via `label`). `known?` concepts are left out, with what only
  they needed."
  ([adj id] (before adj id (constantly false) str))
  ([adj id known? label]
   (let [seen (volatile! #{})
         out  (volatile! [])]
     (letfn [(visit [u]
               (when-not (or (@seen u) (known? u))
                 (vswap! seen conj u)
                 (run! visit (sort-by label (adj u)))
                 (vswap! out conj u)))]
       (run! visit (sort-by label (adj id)))
       @out))))

^:rct/test
(comment
  ;; :c needs :b needs :a; :x and :y claim each other; :p needs :q needs :r needs :p,
  ;; and the sources introduce :p on page 1, :q on 2, :r on 9: ":q needs :r" is the
  ;; claim their order contradicts most (:r comes 7 pages after :q)
  (def cs (for [[of needs] [[:c :b] [:b :a] [:x :y] [:y :x] [:p :q] [:q :r] [:r :p] [:c :c]]]
            {:of of :needs needs}))
  (def g (dag cs {:p {:s 1} :q {:s 2} :r {:s 9}}))
  g ;=> {:c #{:b} :b #{:a} :p #{:q} :q #{} :r #{:p}}
  (before g :c) ;=> [:a :b]
  (before g :r) ;=> [:q :p]
  (before g :c #{:a} str) ;=> [:b]
  (before g :a) ;=> []
  )

;;=============================================================================
;; Derived once per db version
;;=============================================================================

(def derived
  "{:claims :pages :dag :why {[of needs] why}} for a db: `claims`,
  `first-pages`, the `dag` and each claim's reason. Every page scoring the
  learner needs them: computed once per db version (db/per-version)."
  (db/per-version
   (fn [db]
     (let [cs (claims db), pages (first-pages db)]
       {:claims cs :pages pages :dag (dag cs pages)
        :why (into {} (map (juxt (juxt :of :needs) :why)) cs)}))))

;;=============================================================================
;; Judging (model-written)
;;=============================================================================

(def ^:private judge-instructions
  "You map what a learner must understand before what, in a knowledge base built
from documents. Each document is split into passages tagged with concepts; the
index of every concept (label: gloss) is below.

You receive some concepts, each with its gloss, where the documents introduce
it, its first passages and the concepts it is most discussed with. For each,
answer two questions.

needs: which concepts of the index must a learner understand first, to
understand this one? A concept is needed when the definition of this one uses
it, or this one cannot be explained without it. Not needed: concepts merely
discussed nearby, examples of it, uses of it, its narrower kinds, siblings, or
results proved with it. A broader concept is needed only when this one is
defined in terms of it. Name direct needs only: if A needs B and B needs C,
A's needs list B, not C. Usually 0 to 3, and none is a common answer: many
concepts can be learned independently of each other. The documents' order is a
hint (an author usually explains a prerequisite earlier), not a rule. Give
each a why of at most 15 words. Use labels exactly as in the index.

background: knowledge this concept assumes that is NOT in the index, which the
documents take for granted (e.g. \"probability distribution\", \"function\",
\"proof by induction\", \"lambda abstraction\"). Name it as a concept: a
lowercase singular label, a one-sentence gloss, and a why. Only foundations a
reader of the documents might really lack, most often none, rarely more than
two. Never name a concept of the index as background, and never name the
document's field itself (\"game theory\", \"type theory\").

Each index line names the documents mentioning the concept in brackets. A
concept of another document is a valid need only when it is the same idea:
\"type\" in a type theory text is not a player's type in a game.

Concept index:
")

(defn- catalogue
  "The concept index for the system prompt: label [the sources mentioning
  it]: gloss, sorted, so a label two fields share is told apart. The same
  for every call of a run, so it is cached."
  [concepts pages short]
  (->> (vals concepts)
       (sort-by (comp norm :concept/label))
       (map (fn [{:concept/keys [id label gloss]}]
              (str "- " label
                   " [" (if-let [ss (seq (keys (pages id)))] (str/join ", " (sort (map short ss))) "no passage") "]"
                   (when gloss (str ": " gloss)))))
       (str/join "\n")))

(def ^:private judge-schema
  {:type "object"
   :properties
   {:concepts {:type  "array"
               :items {:type "object"
                       :properties {:concept    {:type "string"}
                                    :needs      {:type  "array"
                                                 :items {:type "object"
                                                         :properties {:label {:type "string"} :why {:type "string"}}
                                                         :required ["label" "why"] :additionalProperties false}}
                                    :background {:type  "array"
                                                 :items {:type "object"
                                                         :properties {:label {:type "string"} :gloss {:type "string"}
                                                                      :why {:type "string"}}
                                                         :required ["label" "gloss" "why"] :additionalProperties false}}}
                       :required ["concept" "needs" "background"]
                       :additionalProperties false}}}
   :required ["concepts"]
   :additionalProperties false})

(defn- source-name
  "A source's short name: its title up to a colon."
  [titles id]
  (-> (titles id "?") (str/split #":") first str/trim))

(defn- clip [s n] (if (> (count s) n) (str (subs s 0 n) "…") s))

(defn- first-passages
  "The `n` earliest passages (paragraph groups, else any node) mentioning
  concept `id`: [{:title :page :text}]."
  [db {:keys [mentions]} id n]
  (->> (mentions id)
       (map #(d/pull db [:node/title :node/kind :node/page :node/summary :node/text] [:node/id %]))
       (filter :node/page)
       (sort-by (juxt :node/page #(if (= :group (:node/kind %)) 0 1)))
       (take n)
       (mapv (fn [{:node/keys [title page summary text]}]
               {:title title :page page :text (clip (or summary text "") 500)}))))

(defn- concept-brief
  "The user-prompt section about concept `id`."
  [db {:keys [concepts] :as g} adj pages source-title id]
  (let [{:concept/keys [label gloss aliases broader]} (concepts id)
        label-of #(get-in concepts [% :concept/label])
        at       (fn [c] (->> (pages c) (map (fn [[s p]] (str (source-title s) " p. " p))) (str/join "; ")))
        near     (->> (graph/related adj id 12)
                      (filter (comp #{:related} :kind second))
                      (map (fn [[c _]] (str (label-of c)
                                            (when-let [p (some-> (pages c) vals seq (->> (reduce min)))]
                                              (str " (p. " p ")"))))))]
    (str "### " label "\n"
         (when gloss (str "Gloss: " gloss "\n"))
         (when-let [others (seq (sort (disj (set aliases) label)))]
           (str "Also written: " (str/join ", " (take 5 others)) "\n"))
         (when (seq broader) (str "Broader: " (str/join ", " (sort (map label-of broader))) "\n"))
         (when (seq (pages id)) (str "Introduced: " (at id) "\n"))
         (when-let [ps (seq (first-passages db g id 2))]
           (str "First passages:\n"
                (str/join "\n" (for [{:keys [title page text]} ps] (str "- " title " (p. " page "): " text)))
                "\n"))
         (when (seq near) (str "Most discussed with: " (str/join ", " near) "\n")))))

(defn pending
  "Ids of the concepts not judged yet, mentioned by source `source-id`
  (any source when nil), hidden ones aside."
  [db source-id]
  (let [pages (first-pages db)]
    (->> (d/q '[:find [?cid ...] :where [?c :concept/id ?cid]
                [(missing? $ ?c :concept/prereqs-by)] [(missing? $ ?c :concept/origin)]]
              db)
         (filter #(if source-id (get-in pages [% source-id]) (seq (pages %))))
         (sort-by #(or (get-in pages [% source-id]) (some->> (pages %) vals (reduce min)) 0))
         vec)))

(defn- by-name
  "norm name -> concept id, over labels and aliases."
  [concepts]
  (into {} (for [[id c] concepts, n (conj (set (:concept/aliases c)) (:concept/label c))] [(norm n) id])))

(defn judge
  "Judge the concepts `ids`, `batch` per call, `threads` calls at a time.
  Returns {:by model :judged [id ..] :claims [{:of :needs :why}]
  :background [{:of :label :gloss :why}]}: needs naming no concept become
  background. opts: :models, :tier (:common), :batch (8), :threads (4),
  :on-batch (fn [{:done :total}])."
  [db client ids {:keys [models tier batch threads on-batch] :or {tier :common batch 8 threads 4}}]
  (let [{:keys [concepts] :as g} (graph/load-graph db)
        adj     (graph/edges g)
        pages   (first-pages db)
        titles  (into {} (d/q '[:find ?id ?t :where [?s :source/id ?id] [?s :source/title ?t]] db))
        short   #(source-name titles %)
        names   (by-name concepts)
        model   (llm/model-for models tier)
        system  (str judge-instructions (catalogue concepts pages short))
        done    (atom 0)
        pool    (java.util.concurrent.Executors/newFixedThreadPool threads)
        run     (fn [chunk]
                  (let [{reply :concepts}
                        (llm/complete-json client {:system     system
                                                   :prompt     (str (str/join "\n" (map #(concept-brief db g adj pages short %) chunk))
                                                                    "\nFor each concept above: what it needs, and the background it assumes?")
                                                   :model      model
                                                   :max-tokens 8000
                                                   :schema     judge-schema})
                        mine  (set chunk)
                        out   (for [{:keys [concept needs background]} reply
                                    :let [b (names (norm concept))] :when (mine b)]
                                (let [[found missing] ((juxt filter remove) #(names (norm (:label %))) needs)]
                                  {:claims     (for [{:keys [label why]} found
                                                     :let [a (names (norm label))] :when (not= a b)]
                                                 {:of b :needs a :why why})
                                   :background (concat
                                                (for [{:keys [label gloss why]} background
                                                      :let [a (names (norm label))]]
                                                  (if a {:of b :needs a :why why} {:of b :label (norm label) :gloss gloss :why why}))
                                                (for [{:keys [label why]} missing]
                                                  {:of b :label (norm label) :gloss "" :why why}))}))
                        r {:claims     (vec (concat (mapcat :claims out) (filter :needs (mapcat :background out))))
                           :background (vec (remove :needs (mapcat :background out)))}]
                    (when on-batch (on-batch {:done (swap! done + (count chunk)) :total (count ids)}))
                    r))]
    (try
      (let [[c0 & more] (partition-all batch ids)
            ;; the first call alone writes the cached system prompt the others read
            r0 (run (vec c0))
            rs (->> more
                    (mapv (fn [chunk] (.submit pool ^Callable (fn [] (run (vec chunk))))))
                    (mapv #(.get ^java.util.concurrent.Future %))
                    (cons r0))]
        {:by         model
         :judged     (vec ids)
         :claims     (into [] (comp (mapcat :claims) (distinct)) rs)
         :background (into [] (mapcat :background) rs)})
      (finally (.shutdown pool)))))

;;-----------------------------------------------------------------------------
;; hidden concepts

(def ^:private consolidate-instructions
  "You curate the background knowledge a knowledge base assumes. Concepts of
its documents were each judged for knowledge they take for granted that no
concept of the index names. You receive those names, each with the glosses
given and how many concepts need it.

Group the names that mean the same knowledge (spelling, plural, wording, or a
narrow name for what is really the same foundation) into one hidden concept:
a lowercase singular label, a one-sentence gloss, and the names it covers.
Keep different ideas apart even when their words overlap. Every name goes to
exactly one group. When a group is in fact a concept of the index below, give
its index label as existing (else an empty string).

Concept index:
")

(def ^:private consolidate-schema
  {:type "object"
   :properties {:groups {:type  "array"
                         :items {:type "object"
                                 :properties {:label    {:type "string"} :gloss {:type "string"}
                                              :names    {:type "array" :items {:type "string"}}
                                              :existing {:type "string"}}
                                 :required ["label" "gloss" "names" "existing"]
                                 :additionalProperties false}}}
   :required ["groups"]
   :additionalProperties false})

(defn consolidate
  "Merge the background of `judgement` (see `judge`) into hidden concepts.
  Returns the judgement with :hidden [{:label :gloss :needed-by [{:of :why}]}]
  and the background that turned out to be an index concept moved into
  :claims. opts: :models, :tier (:common)."
  [db client {:keys [background] :as judgement} {:keys [models tier] :or {tier :common}}]
  (if (empty? background)
    (assoc judgement :hidden [])
    (let [{:keys [concepts]} (graph/load-graph db)
          titles (into {} (d/q '[:find ?id ?t :where [?s :source/id ?id] [?s :source/title ?t]] db))
          names  (by-name concepts)
          by-lbl (group-by :label background)
          {:keys [groups]}
          (llm/complete-json client {:system     (str consolidate-instructions
                                                      (catalogue concepts (first-pages db) #(source-name titles %)))
                                     :prompt     (str "Background names (concepts needing it; glosses given):\n"
                                                      (str/join "\n" (for [[l bs] (sort-by key by-lbl)]
                                                                       (str "- \"" l "\" (" (count bs) "; "
                                                                            (->> bs (map :gloss) (remove str/blank?) distinct (take 2) (str/join " | "))
                                                                            ")"))))
                                     :model      (llm/model-for models tier)
                                     :max-tokens 16000
                                     :schema     consolidate-schema})
          ;; each name to its first group
          [groups _] (reduce (fn [[out taken] g]
                               (let [ns (->> (:names g) (map norm) (filter by-lbl) (remove taken) distinct vec)]
                                 [(cond-> out (seq ns) (conj (assoc g :names ns))) (into taken ns)]))
                             [[] #{}] groups)
          grouped (set (mapcat :names groups))
          ;; a name the model left out is a group of its own
          groups  (into groups (for [l (keys by-lbl) :when (not (grouped l))]
                                 {:label l :gloss (some #(not-empty (:gloss %)) (by-lbl l)) :names [l] :existing ""}))
          needers (fn [g] (->> (:names g) (mapcat by-lbl) (map #(select-keys % [:of :why]))
                               (reduce (fn [m n] (cond-> m (not (m (:of n))) (assoc (:of n) n))) {}) vals vec))
          target  (fn [g] (or (names (norm (:existing g))) (names (norm (:label g)))))]
      (-> judgement
          (dissoc :background)
          (update :claims into (for [g groups :let [a (target g)] :when a, n (needers g) :when (not= a (:of n))]
                                 (assoc n :needs a)))
          (assoc :hidden (vec (for [g groups :when (not (target g))]
                                {:label (norm (:label g)) :gloss (:gloss g) :needed-by (needers g)})))))))

;;-----------------------------------------------------------------------------
;; storing

(defn ->tx
  "Tx data for a consolidated `judgement`: hidden concepts, the claims, each
  judged concept marked, and an :edit/* entry."
  [{:keys [by judged claims hidden]}]
  (let [ref  (fn [id] [:concept/id id])
        tmp  #(str "hidden:" %)]
    (vec (concat
          (for [{:keys [label gloss]} hidden]
            (cond-> {:db/id (tmp label) :concept/id (random-uuid) :concept/label label
                     :concept/aliases [label] :concept/origin :inferred :concept/by by}
              (not (str/blank? gloss)) (assoc :concept/gloss gloss)))
          (for [{:keys [of needs why]} claims]
            {:prereq/of (ref of) :prereq/needs (ref needs) :prereq/why why :prereq/by by})
          (for [{:keys [label needed-by]} hidden, {:keys [of why]} needed-by]
            {:prereq/of (ref of) :prereq/needs (tmp label) :prereq/why why :prereq/by by})
          (for [id judged] [:db/add (ref id) :concept/prereqs-by by])
          [{:edit/op :prereqs :edit/by by :edit/at (java.util.Date.)
            :edit/args {:judged (count judged) :claims (count claims) :hidden (mapv :label hidden)}}]))))

(defn judge!
  "Judge the pending concepts of source `source-id` (all when nil), merge
  their background into hidden concepts and store it all. opts as `judge`.
  Returns {:judged n :claims n :hidden n}."
  [conn client source-id opts]
  (let [db  (d/db conn)
        ids (pending db source-id)]
    (if (empty? ids)
      {:judged 0 :claims 0 :hidden 0}
      (let [j (consolidate db client (judge db client ids opts) opts)]
        ;; a keyword ignored as too generic that now names hidden background is ignored no more
        (d/transact! conn (into (->tx j)
                                (for [{:keys [label]} (:hidden j)
                                      :let [e (d/entid db [:ignored/keyword label])] :when e]
                                  [:db/retractEntity e])))
        {:judged (count ids) :claims (+ (count (:claims j)) (reduce + (map (comp count :needed-by) (:hidden j))))
         :hidden (count (:hidden j))}))))
