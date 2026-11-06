(ns learner.ask
  "Ask the knowledge base (graph RAG).

  The knowledge store is the source of truth: answers come from its nodes,
  across every source, and the original documents are only referred to.

    retrieve   full-text search (titles, keywords, summaries, paragraph
               text) seeds a ranking; scores spread along the graph to
               parents (a section whose parts match is relevant as a
               whole) and keyword links; each source's birdview is always
               included as overview
    answer     Claude (the :common tier) answers from the numbered entries
               only, citing them [n]
    store      the question, answer and cited node ids; reading it back
               rebuilds the cited subgraph from the current graph, so it is
               drilled into like the whole one"
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [learner.llm :as llm]))

;;=============================================================================
;; Retrieval
;;=============================================================================

(def ^:private attr-weight
  {:node/keywords 1.5 :node/title 1.2 :node/summary 1.0 :node/text 1.0})

(defn- search-query
  "Datalevin's query parser wants words; drop punctuation and symbols."
  [q]
  (-> q (str/replace #"[^\p{L}\p{N}\s\-]" " ") str/trim))

(defn- fulltext-scores
  "eid -> score: reciprocal rank of each full-text hit, weighted by the
  attribute it hit, summed over a node's attributes."
  [db q top]
  (let [q (search-query q)]
    (if (str/blank? q)
      {}
      (->> (d/fulltext-datoms db q {:top top})
           (map-indexed (fn [rank [e a _]] [e (* (attr-weight a 1.0) (/ 1.0 (+ 5 rank)))]))
           (reduce (fn [m [e s]] (update m e (fnil + 0.0) s)) {})))))

(def ^:private spread {:parent 0.35 :link 0.2})

(defn- neighbours [db eid]
  (let [n (d/pull db [{:node/_children [:db/id]} {:node/links [:db/id]}] eid)]
    {:parent (some-> n :node/_children first :db/id)
     :links  (map :db/id (:node/links n))}))

(defn- spread-scores
  "Add a share of each hit's score to its parent and its linked nodes."
  [db scores]
  (reduce-kv (fn [acc e s]
               (let [{:keys [parent links]} (neighbours db e)]
                 (cond-> (reduce #(update %1 %2 (fnil + 0.0) (* s (:link spread))) acc links)
                   parent (update parent (fnil + 0.0) (* s (:parent spread))))))
             scores
             scores))

(def ^:private node-pull
  [:db/id :node/id :node/kind :node/level :node/order :node/title :node/summary :node/text
   :node/page :node/keywords {:node/source [:source/id :source/title]}])

(defn- path-of
  "Ancestors of `eid`, root first: [{:node/id :node/title :node/kind :node/order :node/page} ...]."
  [db eid]
  (loop [eid eid acc ()]
    (if-let [pid (some-> (d/pull db [{:node/_children [:db/id]}] eid) :node/_children first :db/id)]
      (recur pid (conj acc (d/pull db [:db/id :node/id :node/title :node/kind :node/order :node/page] pid)))
      (vec acc))))

(defn- entry-size [{:node/keys [title summary text]}]
  (+ (count title) (count summary) (min 2000 (count text))))

(defn retrieve
  "Rank the knowledge base for `question`. Returns
  {:overview [root ...] :ranked [{:score .. :node ..} ...]}, :ranked cut to
  `max-nodes` / `max-chars`, highest score first."
  [db question {:keys [top max-nodes max-chars] :or {top 40 max-nodes 24 max-chars 40000}}]
  (let [roots  (d/q '[:find [?r ...] :where [_ :source/root ?r]] db)
        root?  (set roots)
        scores (->> (fulltext-scores db question top)
                    (spread-scores db)
                    (remove (comp root? key))
                    (sort-by val >))]
    {:overview (mapv #(d/pull db node-pull %) roots)
     :ranked   (loop [[[e s] & more] scores, n 0, chars 0, out []]
                 (if (or (nil? e) (>= n max-nodes) (>= chars max-chars))
                   out
                   (let [node (d/pull db node-pull e)]
                     (recur more (inc n) (+ chars (entry-size node))
                            (conj out {:score s :node node})))))}))

;;=============================================================================
;; Answer
;;=============================================================================

(def ^:private system-prompt
  "You answer questions from a personal knowledge base. It was built by folding
documents the user studies into a graph: each document has an overview, chapters
and sections with summaries, passages, and the original paragraphs.

The knowledge base is the source of truth. Answer only from the numbered entries
in the request, never from outside knowledge, even when you know the topic. Each
entry shows where it sits (document > chapter > section), its kind, page, and its
summary or original text.

- Cite the entries that support each claim with [n] right after the claim, one
  number per bracket: [2][5], not [2, 5].
- Prefer original paragraphs for precise statements (definitions, lemmas,
  formulas) and summaries for overviews.
- If the entries only partly answer the question, answer that part and say what
  is missing. If they do not answer it at all, say so in one or two sentences and
  name what the knowledge base does cover nearby. Never guess.
- Entries may come from several documents; combine them, and say so when they
  disagree.
- Plain prose in short paragraphs separated by blank lines. No headings, no bold,
  no lists unless the question asks for steps or items. Keep the documents'
  notation. Usually one to three paragraphs.")

(def ^:private answer-schema
  {"type"                 "object"
   "properties"           {"title"     {"type" "string" "description" "The answer's gist as a short title, at most 8 words"}
                           "answer"    {"type" "string"}
                           "followups" {"type"        "array"
                                        "items"       {"type" "string"}
                                        "description" "Up to 3 follow-up questions the entries suggest"}}
   "required"             ["title" "answer" "followups"]
   "additionalProperties" false})

(defn- entry-text
  "One numbered context entry."
  [i {:node/keys [kind title summary text page keywords source]} path]
  (let [where (if (= :source kind)
                (str (:source/title source) " (overview)")
                (str/join " › " (concat [(:source/title source)]
                                        (keep :node/title (rest path))
                                        (when-not (= :paragraph kind) [title]))))
        body  (if (= :paragraph kind)
                (let [t (or text "")] (if (> (count t) 2000) (str (subs t 0 2000) " …") t))
                summary)]
    (str "[" (inc i) "] " where
         " (" (name kind) (when page (str ", p. " page)) ")\n"
         body
         (when (and (seq keywords) (not= :paragraph kind))
           (str "\nKeywords: " (str/join ", " (sort keywords)))))))

(defn- prompt [question entries]
  (str "Knowledge base entries:\n\n"
       (str/join "\n\n" (map-indexed (fn [i {:keys [node path]}] (entry-text i node path)) entries))
       "\n\nQuestion: " question))

(defn renumber
  "Citations in `answer` refer to entry numbers 1..n-entries. Renumber them in
  order of first use, dropping invalid ones. Returns [answer' [entry-index ...]]
  (0-based indexes, in the new numbering's order)."
  [answer n-entries]
  (let [order (volatile! [])
        text  (str/replace answer #"\[(\d+(?:\s*,\s*\d+)*)\]"
                           (fn [[_ nums]]
                             (->> (str/split nums #"\s*,\s*")
                                  (map parse-long)
                                  (filter #(<= 1 % n-entries))
                                  (map (fn [k]
                                         (let [i (dec k)]
                                           (when-not (some #{i} @order) (vswap! order conj i))
                                           (str "[" (inc (.indexOf ^java.util.List @order i)) "]"))))
                                  (apply str))))]
    [(-> text
         (str/replace #"[ \t]+(\[\d+\])" "$1") ; citations sit against the word they support
         (str/replace #"[ \t]+([.,;:])" "$1")
         (str/replace #"[ \t]{2,}" " ")
         str/trim)
     @order]))

^:rct/test
(comment
  (renumber "A [3]. B [1, 3] and [9]. C [2][3]." 3)
  ;=> ["A[1]. B[2][1] and. C[3][1]." [2 0 1]]
  (renumber "Nothing cited." 4) ;=> ["Nothing cited." []]
  )

(defn ask!
  "Answer `question` from the knowledge base in `conn` and store it.
  opts: :models tier overrides, :context retrieval limits (see retrieve).
  Returns the new ask's id."
  [conn client question {:keys [models context]}]
  (let [db      (d/db conn)
        {:keys [overview ranked]} (retrieve db question context)
        entries (mapv (fn [node] {:node node :path (path-of db (:db/id node))})
                      (concat overview (map :node ranked)))
        model   (llm/model-for models :common)
        {:keys [title answer followups]}
        (llm/complete-json client {:system     system-prompt
                                   :prompt     (prompt question entries)
                                   :model      model
                                   :max-tokens 4000
                                   :schema     answer-schema})
        [answer cited] (renumber answer (count entries))
        cited-ids (mapv #(get-in entries [% :node :node/id]) cited)
        id        (random-uuid)]
    (d/transact! conn [{:ask/id        id
                        :ask/question  question
                        :ask/title     title
                        :ask/answer    answer
                        :ask/cited     cited-ids
                        :ask/matches   (->> ranked (map (comp :node/id :node))
                                            (remove (set cited-ids)) (take 12) vec)
                        :ask/followups (vec (take 3 followups))
                        :ask/model     model
                        :ask/at        (java.util.Date.)}])
    id))

;;=============================================================================
;; Reading back
;;=============================================================================

(defn subgraph
  "Merge root-first `paths` (one per cited node, ending with it) into a
  forest. `cite` maps node id -> citation number. Children keep document
  order (:node/order)."
  [paths cite]
  (letfn [(build [paths]
            (->> paths
                 (group-by (comp :node/id first))
                 (map (fn [[_ ps]] [(apply merge (map first ps)) ps]))
                 (sort-by (fn [[n _]] (or (:node/order n) 0)))
                 (mapv (fn [[n ps]]
                         (let [rest-paths (keep next ps)]
                           (cond-> (select-keys n [:node/id :node/title :node/kind :node/page
                                                   :node/summary :node/text])
                             (cite (:node/id n)) (assoc :cite (cite (:node/id n)))
                             (seq rest-paths)    (assoc :children (build rest-paths))))))))]
    (build paths)))

^:rct/test
(comment
  (def root {:node/id :r :node/title "Doc"})
  (def ch1 {:node/id :c1 :node/title "Ch 1" :node/order 0})
  (def ch2 {:node/id :c2 :node/title "Ch 2" :node/order 1})
  (def p {:node/id :p :node/title "Para" :node/order 3})
  ;; cited: :p (1) and :c1 (2); :c2 listed first but sorts after :c1
  (subgraph [[root ch2 p] [root ch1]] {:p 1 :c1 2})
  ;=> [{:node/id :r :node/title "Doc"
  ;     :children [{:node/id :c1 :node/title "Ch 1" :cite 2}
  ;                {:node/id :c2 :node/title "Ch 2"
  ;                 :children [{:node/id :p :node/title "Para" :cite 1}]}]}]

  ;; a cited node that is also on another cited node's path appears once
  (subgraph [[root (assoc ch1 :node/summary "S")] [root ch1 p]] {:c1 1 :p 2})
  ;=> [{:node/id :r :node/title "Doc"
  ;     :children [{:node/id :c1 :node/title "Ch 1" :node/summary "S" :cite 1
  ;                 :children [{:node/id :p :node/title "Para" :cite 2}]}]}]
  )

(def ^:private ref-pull
  [:node/id :node/title :node/kind :node/level :node/page :node/summary :node/text
   {:node/source [:source/id :source/title]}])

(defn- ref-of
  "Node ref with its path (titles, root first) for display; nil when the
  node is gone (its source was re-ingested)."
  [db id]
  (when-let [eid (d/entid db [:node/id id])]
    (assoc (d/pull db ref-pull eid)
           :node/path (mapv #(select-keys % [:node/id :node/title]) (path-of db eid)))))

(defn view
  "A stored ask for display: the answer, the cited nodes (in citation order;
  nil for ones no longer in the store), their subgraph, other matches."
  [db id]
  (when-let [eid (d/entid db [:ask/id id])]
    (let [a     (d/pull db '[*] eid)
          cited (:ask/cited a)
          paths (for [cid cited
                      :let [eid (d/entid db [:node/id cid])]
                      :when eid]
                  (conj (path-of db eid) (d/pull db [:node/id :node/title :node/kind :node/order
                                                     :node/page :node/summary :node/text]
                                                 eid)))]
      (-> (dissoc a :db/id :ask/model)
          (assoc :ask/cited (mapv #(ref-of db %) cited)
                 :ask/tree (subgraph paths (zipmap cited (map inc (range))))
                 :ask/matches (vec (keep #(ref-of db %) (:ask/matches a))))))))

(defn history
  "Recent asks, newest first."
  [db n]
  (->> (d/q '[:find [(pull ?a [:ask/id :ask/question :ask/title :ask/at]) ...]
              :where [?a :ask/id]]
            db)
       (sort-by :ask/at #(compare %2 %1))
       (take n)
       vec))

(defn search
  "Full-text search without the model: ranked node refs with their paths."
  [db q n]
  (->> (:ranked (retrieve db q {:max-nodes n}))
       (keep #(ref-of db (get-in % [:node :node/id])))
       vec))
