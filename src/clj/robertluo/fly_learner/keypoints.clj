(ns robertluo.fly-learner.keypoints
  "Key sentences: the few sentences of a passage that carry it (a definition,
  the statement of a result, the claim argued, the conclusion), each with a
  short label: the headings the passage would have if it had any. Shown
  highlighted in the source, so a page reads like an outline.

    extract!    the :common model reads passages (paragraph groups) a few at
                a time, with their paragraphs numbered, and quotes 1-3
                sentences each, verbatim, with a label; a quote is kept only
                when found in its paragraph (whitespace and case aside), as
                the paragraph's own text. Stored on the passage as
                :node/keypoints [{:para node-id :quote str :label str}]
                ([] when none), so a re-ingest drops them with the passage
                and the next pass makes them again
    for-source  every key sentence of a source with its page, for the reader"
  (:require [robertluo.fly-learner.prompts :as prompts]
            [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.llm :as llm]))

;;=============================================================================
;; Finding a quote in its paragraph
;;=============================================================================

(defn- squeezed
  "`s` without whitespace, lower case, with for each of its chars the index
  in `s` it came from."
  [s]
  (let [kept (keep-indexed (fn [i c] (when-not (Character/isWhitespace (char c)) [i c])) s)]
    [(str/lower-case (apply str (map second kept))) (mapv first kept)]))

(defn locate
  "The text of `paragraph` that `quote` copies (whitespace and case aside;
  a hyphen the model dropped at a line break too), or nil."
  [paragraph quote]
  (let [[p idx] (squeezed paragraph)
        try-q   (fn [q] (let [[q _] (squeezed q)]
                          (when (>= (count q) 12)
                            (when-let [i (str/index-of p q)]
                              (subs paragraph (idx i) (inc (idx (+ i (count q) -1))))))))]
    (or (try-q quote)
        ;; "ra- tional" in the paragraph, "rational" in the quote: try without hyphens on both sides
        (let [unhyphen #(str/replace % #"-\s+" "")
              p'       (unhyphen paragraph)]
          (when (not= p' paragraph)
            (when-let [found (locate p' quote)]
              ;; map back: the paragraph's own text from the found one's first and last words
              (let [words (str/split (str/trim found) #"\s+")
                    start (str/index-of paragraph (first words))
                    end   (some-> (str/last-index-of paragraph (last words)) (+ (count (last words))))]
                (when (and start end (< start end)) (subs paragraph start end)))))))))

^:rct/test
(comment
  (locate "We say that a strategy  is\nstrictly dominated if another one is better." "a strategy is strictly dominated if another")
  ;=> "a strategy  is\nstrictly dominated if another"
  (locate "Players are ra- tional and know it." "Players are rational and know it.") ;=> "Players are ra- tional and know it."
  (locate "Short text." "Something else entirely here") ;=> nil
  )

;;=============================================================================
;; Extracting
;;=============================================================================

(def ^:private system-prompt
  (prompts/prompt!
   :keypoints/pick
   {:group "Reading" :title "Pick the key sentences"
    :about "After an ingest, a few passages a call (common tier): the 1-3 sentences that carry each, with a short label. The reply is JSON; a quote not found word for word in its paragraph is dropped."
    :default
    "You read passages of documents a learner studies and pick, in each, the sentences
that carry it: the ones a reader skimming the page should not miss. They are the
passage's hidden headings.

Pick 1 to 3 sentences per passage (1 for a short one), in order:
- a definition, or the statement of a theorem, lemma, rule or result
- the central claim the passage argues, or its conclusion
- the answer to the question the passage raises
Not: examples' details, notation alone, transitions (\"We now turn to...\"),
references to other sections, exercises. A passage with nothing worth it (front
matter, a list of references, garbled formulas) gets none.

For each, quote the sentence exactly as written in the paragraph, character for
character, odd spacing, hyphens and formula fragments included: it is searched
for in the text. A long sentence may be cut to the clause that carries it (at
least 6 words). Give the paragraph's number, and a label: 2 to 6 words naming its
point like a heading, in the document's terms (\"IDSDS output survives
rationality\", \"Definition of strict dominance\")."}))

(def ^:private schema
  {"type" "object"
   "properties"
   {"passages" {"type" "array"
                "items" {"type" "object"
                         "properties" {"passage" {"type" "integer"}
                                       "points"  {"type" "array"
                                                  "items" {"type" "object"
                                                           "properties" {"paragraph" {"type" "integer"}
                                                                         "sentence"  {"type" "string"}
                                                                         "label"     {"type" "string"}}
                                                           "required" ["paragraph" "sentence" "label"]
                                                           "additionalProperties" false}}}
                         "required" ["passage" "points"]
                         "additionalProperties" false}}}
   "required" ["passages"]
   "additionalProperties" false})

(defn- paragraphs-of [db gid]
  (->> (d/pull db [{:node/children [:node/id :node/order :node/text :node/kind]}] [:node/id gid])
       :node/children (filter #(and (= :paragraph (:node/kind %)) (:node/text %))) (sort-by :node/order) vec))

(defn- passage-text [db i gid]
  (let [{:node/keys [title summary]} (d/pull db [:node/title :node/summary] [:node/id gid])]
    (str "## Passage " (inc i) ": " title "\n"
         (when summary (str "Summary: " summary "\n"))
         (str/join "\n" (map-indexed (fn [j p] (str "[" (inc j) "] " (:node/text p))) (paragraphs-of db gid))))))

(defn pending
  "Ids of the passages (paragraph groups) without key sentences yet, of
  source `source-id` (all sources when nil), in reading order."
  [db source-id]
  (->> (d/q '[:find ?id ?sid ?o :where [?g :node/kind :group] [?g :node/id ?id] [?g :node/order ?o]
              [?g :node/source ?s] [?s :source/id ?sid] [(missing? $ ?g :node/keypoints)]]
            db)
       (filter #(or (nil? source-id) (= source-id (second %))))
       (sort-by (juxt second #(nth % 2)))
       (mapv first)))

(defn- extract-batch
  "{group-id [keypoint ..]} for the passages `gids` (one model call)."
  [db client model gids]
  (let [{:keys [passages]}
        (llm/complete-json client {:system     (prompts/text system-prompt)
                                   :prompt     (str (str/join "\n\n" (map-indexed #(passage-text db %1 %2) gids))
                                                    "\n\nThe key sentences of each passage?")
                                   :model      model
                                   :max-tokens 6000
                                   :schema     schema})
        by-passage (into {} (map (juxt :passage :points)) passages)]
    (into {}
          (map-indexed
           (fn [i gid]
             (let [paras (paragraphs-of db gid)]
               [gid (vec (for [{:keys [paragraph sentence label]} (take 3 (by-passage (inc i)))
                               :let [p (get paras (dec paragraph))
                                     q (some-> p :node/text (locate sentence))]
                               :when q]
                           {:para (:node/id p) :quote q :label (str/trim label)}))])))
          gids)))

(defn extract!
  "Find the key sentences of the passages without them, `batch` per call,
  `threads` calls at a time. opts: :models, :source (a source id: only it),
  :batch (5), :threads (4), :on-batch (fn [{:done :total :found}]). Returns
  {:passages n :keypoints n :dropped n} (dropped: quotes not found in their
  paragraph)."
  [conn client {:keys [models source batch threads on-batch] :or {batch 5 threads 4}}]
  (let [db     (d/db conn)
        gids   (pending db source)
        model  (llm/model-for models :common)
        chunks (partition-all batch gids)
        done   (atom {:done 0 :total (count gids) :found 0})
        pool   (java.util.concurrent.Executors/newFixedThreadPool threads)]
    (try
      (->> chunks
           (mapv (fn [chunk]
                   (.submit pool ^Callable
                            (fn []
                              (let [found (extract-batch db client model (vec chunk))]
                                (d/transact! conn (mapv (fn [[gid kps]] {:node/id gid :node/keypoints kps}) found))
                                (let [s (swap! done #(-> % (update :done + (count chunk))
                                                         (update :found + (reduce + (map count (vals found))))))]
                                  (when on-batch (on-batch s)))
                                found)))))
           (mapv #(.get ^java.util.concurrent.Future %))
           (reduce merge {})
           ((fn [all] {:passages (count all) :keypoints (reduce + (map count (vals all)))})))
      (finally (.shutdown pool)))))

;;=============================================================================
;; Reading
;;=============================================================================

(defn for-source
  "Every key sentence of source `source-id`, in reading order:
  [{:page :para :node :quote :label}] (:node the passage)."
  [db source-id]
  (->> (d/q '[:find ?gid ?o ?kps :in $ ?sid
              :where [?s :source/id ?sid] [?g :node/source ?s] [?g :node/keypoints ?kps]
              [?g :node/id ?gid] [?g :node/order ?o]]
            db source-id)
       (sort-by second)
       (mapcat (fn [[gid _ kps]]
                 (for [{:keys [para quote label]} kps
                       :let [page (:node/page (d/pull db [:node/page] [:node/id para]))]
                       :when page]
                   {:page page :para para :node gid :quote quote :label label})))
       vec))
