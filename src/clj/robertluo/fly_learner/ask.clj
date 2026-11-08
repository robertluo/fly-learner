(ns robertluo.fly-learner.ask
  "Ask the knowledge base (graph RAG).

  The knowledge store is the source of truth: answers come from its nodes,
  across every source, and the original documents are only referred to.

    retrieve   full-text search (titles, keywords, summaries, paragraph
               text) seeds a ranking; a numbered label in the question
               (\"Exercise 1.1\", \"Definition 1.8\") finds the paragraphs
               starting with it first (and the few after them), then those
               mentioning it (full-text sees only \"exercise 1 1\"); the
               full-text scores spread along the graph to
               parents (a section whose parts match is relevant as a
               whole) and keyword links; each source's birdview is always
               included as overview
    answer     Claude (the :common tier) answers from the numbered entries
               only, citing them [n]
    focus      a question may be about a passage the learner selected while
               reading: the nodes it is in come first, the passage is quoted,
               and notation or background the entries take for granted may
               be explained, marked as such
    store      the question, answer and cited node ids; reading it back
               rebuilds the cited subgraph from the current graph, so it is
               drilled into like the whole one"
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.llm :as llm]))

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

(def ^:private label-kinds
  "Words naming the numbered things of a text."
  #{"exercise" "exercises" "problem" "definition" "theorem" "lemma" "proposition" "corollary" "example"
    "remark" "table" "figure" "fig" "section" "chapter" "appendix" "equation" "eq" "axiom" "assumption"
    "algorithm" "claim" "conjecture" "note"})

(defn labels
  "The numbered labels `q` names, as [kind [n ..]]: \"Exercise 1.1\" ->
  [\"exercise\" [\"1\" \"1\"]]."
  [q]
  (vec (distinct (for [[_ kind num] (re-seq #"(?i)\b([a-z]+)\.?\s+(\d+(?:\s*\.\s*\d+)*)\b" q)
                       :let [kind (str/lower-case kind)]
                       :when (label-kinds kind)]
                   [(str/replace kind #"s$" "") (re-seq #"\d+" num)]))))

(defn- label-re
  "Matches label [kind nums] where it starts a text (`start?`) or anywhere;
  1.1 is not 1.10 nor 1.1.2."
  [[kind nums] start?]
  (re-pattern (str "(?i)" (if start? "^\\s*" "\\b") kind "s?\\.?\\s+" (str/join "\\s*\\.\\s*" nums)
                   "(?!\\d|\\s*\\.\\s*\\d)")))

^:rct/test
(comment
  (labels "what does exercise 1.1 ask? and Def. 2, Theorem 3.4") ;=> [["exercise" ["1" "1"]] ["theorem" ["3" "4"]]]
  (labels "the 2 players") ;=> []
  (mapv #(boolean (re-find (label-re ["exercise" ["1" "1"]] true) %))
        ["Exercise 1.1. Antonia" "Exercise 1.10. Find" "Exercise 1 .1 (a)" "See Exercise 1.1." "Exercise 1.1.2"])
  ;=> [true false true false false]
  )

(def ^:private label-limits {:mentions 10 :after 5})

(defn- label-scores
  "eid -> score for the labels `q` names: a paragraph or title starting with
  one about 3 (earlier pages a little more), the paragraphs after it (its
  continuation) about 2, one mentioning it 1: above full-text scores, so
  they come first, in this order."
  [db q]
  (if-let [ls (seq (labels q))]
    (let [texts (concat (d/datoms db :ave :node/text) (d/datoms db :ave :node/title))
          hits  (fn [re] (distinct (keep (fn [[e _ v]] (when (re-find re v) e)) texts)))
          after (fn [e]
                  (when-let [parent (some-> (d/pull db [{:node/_children [:db/id]}] e) :node/_children first :db/id)]
                    (let [sibs (->> (d/pull db [{:node/children [:db/id :node/order]}] parent)
                                    :node/children (sort-by :node/order) (mapv :db/id))]
                      (->> sibs (drop-while #(not= e %)) rest (take (:after label-limits))))))]
      (reduce (fn [m l]
                (let [starts  (sort-by #(:node/page (d/pull db [:node/page] %) 0) (hits (label-re l true)))
                      starts? (set starts)]
                  (as-> m m
                    ;; earlier first: an exercise before its solution
                    (reduce-kv (fn [m i e] (update m e (fnil max 0.0) (- 3.0 (* 0.02 (min i 10))))) m (vec starts))
                    (reduce (fn [m e] (reduce-kv (fn [m i a] (update m a (fnil max 0.0) (- 2.0 (* 0.1 i))))
                                                 m (vec (after e))))
                            m starts)
                    (reduce #(update %1 %2 (fnil max 0.0) 1.0) m
                            (take (:mentions label-limits) (remove starts? (hits (label-re l false))))))))
              {} ls))
    {}))

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
                    ;; after spreading: a section of labelled paragraphs is not the label
                    (merge-with max (label-scores db question))
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
in the request, never from outside knowledge, even when you know the topic (one
exception below, for a selected passage). Each entry shows where it sits
(document > chapter > section), its kind, page, and its summary or original text.

The learner may have selected a passage while reading and ask about it: to
explain it, unpack its notation, give an example, relate it to something else.
Then the question is about that passage; the entries it was selected from come
first. Explain it from the entries and cite them. Where the passage relies on
something the entries never explain (standard notation, a well-known definition
or theorem), you may explain that from general knowledge, briefly, and say plainly
that it is background, not from their documents. The passage may be copied from a
PDF, with formulas garbled; read it together with the entries it came from.

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

(def ^:private max-quote 2000)

(defn- prompt
  "The user prompt: numbered entries, then the question. With a `quote`, the
  passage the question is about and the entries (`from`: 1-based numbers) it
  was selected from."
  [question entries {:keys [quote from]}]
  (str "Knowledge base entries:\n\n"
       (str/join "\n\n" (map-indexed (fn [i {:keys [node path]}] (entry-text i node path)) entries))
       (when quote
         (str "\n\nThe learner is reading"
              (if (seq from) (str " " (str/join "" (map #(str "[" % "]") from))) " these documents")
              " and selected this passage:\n\"\"\"\n"
              (cond-> quote (> (count quote) max-quote) (-> (subs 0 max-quote) (str " …")))
              "\n\"\"\""))
       "\n\n" (if quote "Question about the passage: " "Question: ") question))

^:rct/test
(comment
  (def e [{:node {:node/kind :paragraph :node/title "P" :node/text "Bool := ..." :node/page 86
                  :node/source {:source/title "Thesis"}}
           :path [{:node/title "Thesis"}]}])
  (prompt "Why?" e nil)
  ;=> "Knowledge base entries:\n\n[1] Thesis (paragraph, p. 86)\nBool := ...\n\nQuestion: Why?"
  (prompt "Explain it" e {:quote "Bool := ∀X" :from [1]})
  ;=> "Knowledge base entries:\n\n[1] Thesis (paragraph, p. 86)\nBool := ...\n\nThe learner is reading [1] and selected this passage:\n\"\"\"\nBool := ∀X\n\"\"\"\n\nQuestion about the passage: Explain it"
  )

(defn- beside
  "The other paragraphs on the page of `eid` (its source's), at most 6."
  [db eid]
  (let [{:node/keys [page source]} (d/pull db [:node/page {:node/source [:db/id]}] eid)]
    (if-not (and page source)
      []
      (->> (d/q '[:find [?n ...]
                  :in $ ?s ?p ?self
                  :where [?n :node/source ?s] [?n :node/page ?p] [?n :node/kind :paragraph]
                  [(not= ?n ?self)]]
                db (:db/id source) page eid)
           sort ; ingest order is reading order
           (take 6)))))

(defn- focus-entries
  "Entries first: the overview, then the nodes `focus-ids` (where a selected
  passage is), the paragraphs on the same page (a passage often refers to
  the formula or definition next to it), then the ranked nodes not
  already in. Returns [entries from], `from` the 1-based numbers of the
  focus nodes."
  [db overview focus-ids ranked]
  (let [eids  (keep #(d/entid db [:node/id %]) focus-ids)
        focus (map #(d/pull db node-pull %) eids)
        near  (map #(d/pull db node-pull %) (mapcat #(beside db %) eids))
        nodes (->> (concat overview focus near ranked)
                   (reduce (fn [[seen out] n] (if (seen (:db/id n)) [seen out] [(conj seen (:db/id n)) (conj out n)]))
                           [#{} []])
                   second)]
    [(mapv (fn [node] {:node node :path (path-of db (:db/id node))}) nodes)
     (vec (range (inc (count overview)) (+ 1 (count overview) (count focus))))]))

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
  opts: :models tier overrides, :context retrieval limits (see retrieve),
  :focus {:quote passage :nodes [node-id ..]} when the question is about a
  passage the learner selected (in those nodes, if known).
  Returns the new ask's id."
  [conn client question {:keys [models context focus]}]
  (let [db      (d/db conn)
        quote   (some-> (:quote focus) str/trim not-empty)
        {:keys [overview ranked]} (retrieve db (str question (when quote (str " " quote))) context)
        [entries from] (focus-entries db overview (when quote (:nodes focus)) (map :node ranked))
        model   (llm/model-for models :common)
        {:keys [title answer followups]}
        (llm/complete-json client {:system     system-prompt
                                   :prompt     (prompt question entries (when quote {:quote quote :from from}))
                                   :model      model
                                   :max-tokens 4000
                                   :schema     answer-schema})
        [answer cited] (renumber answer (count entries))
        cited-ids (mapv #(get-in entries [% :node :node/id]) cited)
        id        (random-uuid)]
    (d/transact! conn [(cond-> {:ask/id        id
                                :ask/question  question
                                :ask/title     title
                                :ask/answer    answer
                                :ask/cited     cited-ids
                                :ask/matches   (->> ranked (map (comp :node/id :node))
                                                    (remove (set cited-ids)) (take 12) vec)
                                :ask/followups (vec (take 3 followups))
                                :ask/model     model
                                :ask/at        (java.util.Date.)}
                         quote (assoc :ask/quote quote :ask/focus (vec (:nodes focus))))])
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
