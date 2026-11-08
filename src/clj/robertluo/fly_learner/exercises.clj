(ns robertluo.fly-learner.exercises
  "Exercises a source's author wrote for the reader, with the author's
  solutions: questions with an authority no generated one has.

  Found by their labels, in reading order: a line starting
  \"Exercise 1.5.\" (or \"Problem 1.5:\") opens one, running to the next
  label, a heading-like line (\"Exercises for Section ..\", \"The answers to
  the following exercises ..\", \"Appendix ..\") or the chapter's end. A
  label met again after the chapter's \"Solutions to exercises\" opens its
  solution. Then a model (`:strong`: the concepts it picks decide what an
  answer raises; one chapter's exercises a few at a call, against the
  concepts the chapter mentions) names the concepts each
  tests, what it asks in a line, and the key points a full answer covers,
  from the author's solution when there is one.

  Stored as `:exercise/*`, its id derived from the source's id and the
  label, so finding them again, after a re-ingest too, keeps their identity
  (and the answers given to them). A re-ingest drops them with the source
  (robertluo.fly-learner.ingest/remove-source!) and finds them again.
  `:exercise/concepts` follows merges (robertluo.fly-learner.concept)."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.llm :as llm]))

;;=============================================================================
;; Finding them
;;=============================================================================

(def ^:private start-re #"^\s*(?:Exercise|Problem)\s+(\d+(?:\s*\.\s*\d+)+)\s*[.:]")

(defn- tight
  "A label as the PDF may space it (\"2. 1\") made plain: \"2.1\"."
  [label]
  (str/replace label #"\s+" ""))

(def ^:private boundary-re
  #"(?i)^\s*(?:[\d\s.]*[A-Z]\s*\.[\d\s.]*)?(?:exercises\s+for\s+section|the\s+answers\s+to\s+the\s+following|appendix\b)")

(def ^:private solutions-re #"(?i)solutions?\s+to\s+(?:the\s+)?exercises|answers\s+to\s+(?:the\s+)?exercises\b")

(def max-chars
  "An exercise or solution longer than this is cut: a missed boundary."
  8000)

(defn segments
  "Exercises and solutions in `paras`, paragraphs in reading order
  [{:id :page :chapter :text} ..] (:chapter: any value naming the chapter).
  -> [{:label :role (:exercise | :solution) :id :page :text} ..], :id and
  :page of the paragraph it starts in."
  [paras]
  (let [close (fn [{:keys [open] :as st}]
                (cond-> (dissoc st :open)
                  open (update :out conj (update open :text #(let [t (str/trim (str/join "\n" %))]
                                                               (subs t 0 (min max-chars (count t))))))))]
    (-> (reduce
         (fn [st {:keys [id page chapter text]}]
           (let [st (cond-> st
                      (not= chapter (:chapter st)) (-> close (assoc :chapter chapter :solving? false)))]
             (reduce (fn [st line]
                       (cond
                         (re-find solutions-re line)
                         (-> st close (assoc :solving? true))

                         (re-find boundary-re line)
                         (close st)

                         :else
                         (if-let [[_ label] (re-find start-re line)]
                           (-> st close (assoc :open {:label (tight label) :role (if (:solving? st) :solution :exercise)
                                                      :id id :page page :text [line]}))
                           (cond-> st (:open st) (update-in [:open :text] conj line)))))
                     st
                     (str/split-lines (or text "")))))
         {:out [] :chapter ::none :solving? false}
         paras)
        close
        :out)))

(defn pair
  "Exercises from `segments`, each with its solution when there is one (a
  solution with no exercise is dropped, and a label's later exercises are
  echoes): [{:label :id :page :text :solution} ..] in reading order."
  [segs]
  (let [solutions (reduce (fn [m {:keys [label role text]}]
                            (cond-> m (and (= :solution role) (not (m label))) (assoc label text)))
                          {} segs)]
    (->> segs
         (filter #(= :exercise (:role %)))
         (reduce (fn [[seen out] {:keys [label] :as s}]
                   (if (seen label)
                     [seen out]
                     [(conj seen label) (conj out (-> (dissoc s :role)
                                                      (assoc :solution (solutions label))))]))
                 [#{} []])
         second)))

(defn refers
  "Labels of the other exercises `text` (exercise `label`'s) builds on."
  [label text]
  (->> (re-seq #"Exercises?\s+(\d+(?:\s*\.\s*\d+)+)" text)
       (map (comp tight second))
       (remove #{label})
       distinct
       vec))

^:rct/test
(comment
  (def paras
    [{:id 1 :page 50 :chapter :c1 :text "1.E.1 Exercises for Section 1.1: Game frames"}
     {:id 2 :page 50 :chapter :c1 :text "The answers to the following exercises are in Appendix S."}
     {:id 3 :page 50 :chapter :c1 :text "Exercise 1.1. Antonia and Bob cannot decide."}
     {:id 4 :page 50 :chapter :c1 :text "(a) Represent it as a table."}
     {:id 5 :page 51 :chapter :c1 :text "Exercise 1.2. Use the game of Exercise 1.1.\nExercise 1.3: Three players."}
     {:id 6 :page 51 :chapter :c1 :text "(c) Is it efficient?\n1.E.2 . Exercises for Section 1.2: Dominance"}
     {:id 7 :page 52 :chapter :c1 :text "As Exercise 1.2 in Appendix 1.E shows, it is easy."}
     {:id 8 :page 58 :chapter :c1 :text "Appendix 1.S: Solutions to exercises"}
     {:id 9 :page 58 :chapter :c1 :text "Exercise 1.1. (a) The table is as follows."}
     {:id 10 :page 59 :chapter :c1 :text "Exercise 1.3. Each presses."}
     {:id 11 :page 60 :chapter :c2 :text "Exercise 2. 1. A new chapter, spaced as PDFs do."}])
  (mapv (juxt :label :role :id :text) (segments paras))
  ;=> [["1.1" :exercise 3 "Exercise 1.1. Antonia and Bob cannot decide.\n(a) Represent it as a table."]
  ;    ["1.2" :exercise 5 "Exercise 1.2. Use the game of Exercise 1.1."]
  ;    ["1.3" :exercise 5 "Exercise 1.3: Three players.\n(c) Is it efficient?"]
  ;    ["1.1" :solution 9 "Exercise 1.1. (a) The table is as follows."]
  ;    ["1.3" :solution 10 "Exercise 1.3. Each presses."]
  ;    ["2.1" :exercise 11 "Exercise 2. 1. A new chapter, spaced as PDFs do."]]
  (mapv (juxt :label :solution) (pair (segments paras)))
  ;=> [["1.1" "Exercise 1.1. (a) The table is as follows."] ["1.2" nil]
  ;    ["1.3" "Exercise 1.3. Each presses."] ["2.1" nil]]
  (refers "1.5" "Exercise 1.5. For the game of Exercise 1.1 and Exercises 1.2, 1.5") ;=> ["1.1" "1.2"]
  )

(defn- paragraphs
  "Source `source-id`'s paragraphs in reading order, each with the chapter
  (level-1 node) it is in: [{:id :page :chapter :text} ..]."
  [db source-id]
  (let [pull #(d/pull db [:db/id :node/id :node/kind :node/level :node/order :node/page :node/text
                          {:node/children [:db/id :node/order]}] %)
        walk (fn walk [e chapter]
               (let [{:node/keys [kind level children] :as n} (pull e)
                     chapter (if (= 1 level) (:db/id n) chapter)]
                 (if (= :paragraph kind)
                   [{:id (:node/id n) :page (:node/page n) :chapter chapter :text (:node/text n)}]
                   (mapcat #(walk (:db/id %) chapter) (sort-by :node/order children)))))]
    (when-let [root (-> (d/pull db [{:source/root [:db/id]}] [:source/id source-id]) :source/root :db/id)]
      (vec (walk root nil)))))

(defn found
  "The exercises of source `source-id`, as `pair`, each with :refers and
  :chapter (the entity of its chapter, nil outside any)."
  [db source-id]
  (let [paras   (paragraphs db source-id)
        chapter (into {} (map (juxt :id :chapter)) paras)]
    (mapv (fn [{:keys [label text id] :as e}]
            (assoc e :refers (refers label text) :chapter (chapter id)))
          (pair (segments paras)))))

;;=============================================================================
;; Linking them to concepts, with the model
;;=============================================================================

(def ^:private system-prompt
  "You read exercises an author wrote for the readers of their book, each
with the author's solution when there is one, and the concepts of the
chapter they are in. The text comes from a PDF: tables and formulas may be
flattened into runs of numbers and symbols.

For each exercise:
- concepts: the 1-4 concepts of the list it tests most directly: what a
  reader must understand to solve it, not every term it mentions. Copy
  their labels exactly as listed.
- summary: what it asks the reader to do, in one line of at most 14 words,
  e.g. \"Find the Nash equilibria of a three-player envelope game\".
- points: the 2-5 key points a full answer covers, each one short line,
  following the author's solution when given (its results, values and the
  reasoning that gets there); without a solution, as you would solve it.")

(def ^:private schema
  {"type" "object"
   "properties" {"exercises" {"type" "array"
                              "items" {"type" "object"
                                       "properties" {"n"        {"type" "integer"}
                                                     "concepts" {"type" "array" "items" {"type" "string"}}
                                                     "summary"  {"type" "string"}
                                                     "points"   {"type" "array" "items" {"type" "string"}}}
                                       "required" ["n" "concepts" "summary" "points"]
                                       "additionalProperties" false}}}
   "required" ["exercises"]
   "additionalProperties" false})

(defn- chapter-concepts
  "Concepts mentioned in the subtree of node entity `chapter` (the whole
  source when nil): [{:db/id :concept/label :concept/gloss} ..] by label."
  [db source-id chapter]
  (let [nodes (if chapter
                (loop [todo [chapter] acc #{}]
                  (if-let [e (peek todo)]
                    (let [{:keys [db/id node/children]} (d/pull db [:db/id {:node/children [:db/id]}] e)]
                      (recur (into (pop todo) (map :db/id children)) (conj acc id)))
                    acc))
                (set (d/q '[:find [?n ...] :in $ ?sid :where [?s :source/id ?sid] [?n :node/source ?s]]
                          db source-id)))]
    (->> (d/q '[:find ?n ?c :where [?n :node/keywords ?k] [?c :concept/aliases ?k]] db)
         (keep (fn [[n c]] (when (nodes n) c)))
         distinct
         (map #(d/pull db [:db/id :concept/label :concept/gloss] %))
         (sort-by :concept/label)
         vec)))

(defn- exercise-text [i {:keys [label text solution]} by-label]
  (str "## Exercise [" (inc i) "]: " label "\n" text
       (when-let [ctx (seq (keep by-label (refers label text)))]
         (str "\n\n(It builds on: " (str/join "\n" (map #(subs % 0 (min 1500 (count %))) ctx)) ")"))
       "\n\n### The author's solution\n" (or solution "(none given)")))

(defn- link-batch
  "Model's links for exercises `es` (one chapter), against `concepts`."
  [client model concepts es by-label]
  (let [{:keys [exercises]}
        (llm/complete-json client
                           {:system     system-prompt
                            :prompt     (str "# Concepts of the chapter\n"
                                             (str/join "\n" (for [{:concept/keys [label gloss]} concepts]
                                                              (str "- " label (when gloss (str ": " gloss)))))
                                             "\n\n# Exercises\n\n"
                                             (str/join "\n\n" (map-indexed #(exercise-text %1 %2 by-label) es))
                                             "\n\nFor each exercise: the concepts it tests, a summary and the key points.")
                            :model      model
                            :max-tokens 6000
                            :schema     schema})
        by-n     (into {} (map (juxt :n identity)) exercises)
        by-name  (into {} (map (juxt (comp str/lower-case str/trim :concept/label) :db/id)) concepts)]
    (vec (map-indexed (fn [i e]
                        (let [{:keys [concepts summary points]} (by-n (inc i))]
                          (assoc e
                                 :concepts (vec (take 4 (distinct (keep #(by-name (str/lower-case (str/trim %))) concepts))))
                                 :summary  (some-> summary str/trim not-empty)
                                 :points   (vec (take 6 (keep #(some-> % str/trim not-empty) points))))))
                      es))))

(defn exercise-id
  "The id of exercise `label` of source `source-id`: the same each time it is found."
  [source-id label]
  (java.util.UUID/nameUUIDFromBytes (.getBytes (str source-id "#exercise-" label) "UTF-8")))

(defn ->tx
  "Transaction storing exercises of source `source-id` linked by `by`."
  [db source-id by linked]
  (let [s (d/entid db [:source/id source-id])]
    (concat
     (for [{:keys [label id page text solution refers concepts summary points]} linked
           :let [xid (exercise-id source-id label)
                 old (d/entid db [:exercise/id xid])]
           tx  (concat
                (when old (for [c (:exercise/concepts (d/pull db [{:exercise/concepts [:db/id]}] old))]
                            [:db/retract old :exercise/concepts (:db/id c)]))
                [(cond-> {:exercise/id xid :exercise/source s :exercise/label label :exercise/page page
                          :exercise/text text :exercise/refers refers :exercise/points points
                          :exercise/concepts concepts :exercise/by by}
                   (d/entid db [:node/id id]) (assoc :exercise/node [:node/id id])
                   solution (assoc :exercise/solution solution)
                   summary  (assoc :exercise/summary summary))])]
       tx)
     [[:db/add s :source/exercises-by by]])))

(defn pending
  "Ids of the sources whose exercises were not looked for yet."
  [db]
  ;; not `missing?`: with Datalevin 1.2.10 it kept a source just marked
  (let [done (set (d/q '[:find [?id ...] :where [?s :source/exercises-by] [?s :source/id ?id]] db))]
    (vec (remove done (d/q '[:find [?id ...] :where [?s :source/id ?id] [?s :source/root]] db)))))

(defn extract!
  "Find the exercises of the sources not looked at yet (or `:source`, again)
  and link them to concepts, `batch` exercises a call, `threads` calls at a
  time. opts: :models, :tier (:strong), :source, :batch (5), :threads (4).
  Returns {:sources n :exercises n :solutions n}."
  [conn client {:keys [models tier source batch threads] :or {tier :strong batch 5 threads 4}}]
  (let [db    (d/db conn)
        model (llm/model-for models tier)]
    (reduce
     (fn [acc sid]
       (let [es       (found db sid)
             by-label (into {} (map (juxt :label :text)) es)
             chunks   (for [[chapter xs] (group-by :chapter es)
                            :let [cs (chapter-concepts db sid chapter)]
                            chunk (partition-all batch xs)]
                        [cs (vec chunk)])
             pool     (java.util.concurrent.Executors/newFixedThreadPool threads)
             linked   (try
                        (->> chunks
                             (mapv (fn [[cs chunk]]
                                     (.submit pool ^Callable #(link-batch client model cs chunk by-label))))
                             (into [] (mapcat #(.get ^java.util.concurrent.Future %))))
                        (finally (.shutdown pool)))]
         (d/transact! conn (vec (->tx (d/db conn) sid model linked)))
         (-> acc
             (update :sources inc)
             (update :exercises + (count linked))
             (update :solutions + (count (filter :solution linked))))))
     {:sources 0 :exercises 0 :solutions 0}
     (if source [source] (pending db)))))

;;=============================================================================
;; Reading
;;=============================================================================

(def ^:private pull-spec
  [:exercise/id :exercise/label :exercise/page :exercise/text :exercise/solution :exercise/refers
   :exercise/summary :exercise/points
   {:exercise/node [:node/id]} {:exercise/source [:source/id]} {:exercise/concepts [:concept/id]}])

(defn- label-key
  "Labels in numeric order: 1.2 before 1.10."
  [label]
  (mapv parse-long (str/split label #"\.")))

(defn- tidy [{:exercise/keys [node source concepts] :as x}]
  (-> x
      (dissoc :exercise/node :exercise/source)
      (assoc :node/id (:node/id node) :source/id (:source/id source)
             :exercise/concepts (mapv :concept/id concepts))))

(defn of-concept
  "The exercises testing concept `concept-id`, in the book's order, with
  :node/id (the paragraph it starts in, while it is there) and :source/id."
  [db concept-id]
  (->> (d/q '[:find [?x ...] :in $ ?cid :where [?c :concept/id ?cid] [?x :exercise/concepts ?c]] db concept-id)
       (map #(tidy (d/pull db pull-spec %)))
       (sort-by (juxt :source/id (comp label-key :exercise/label)))
       vec))

(defn by-label
  "{label text} of source `source-id`'s exercises."
  [db source-id]
  (into {} (d/q '[:find ?l ?t :in $ ?sid :where [?s :source/id ?sid] [?x :exercise/source ?s]
                  [?x :exercise/label ?l] [?x :exercise/text ?t]]
                db source-id)))

(defn lookup
  "Up to `n` exercises a query names: \"exercise 1.1\" (or \"ex 1.1\",
  \"problem 1.1\") that one first, then the ones it starts (1.10, 1.11 ..);
  \"exercise 1\": chapter 1's. Each with :node/id, :source/id and its
  concepts as [{:concept/id :concept/label}]. nil when it names none."
  [db q n]
  (when-let [[_ num] (re-find #"(?i)^\s*(?:exercises?|ex\.?|problems?)\s*(\d+(?:\.\d+)*)\.?\s*$" (str q))]
    (->> (d/q '[:find ?x ?l :where [?x :exercise/label ?l]] db)
         (keep (fn [[x l]] (cond (= l num) [0 x l]
                                 (str/starts-with? l (if (str/includes? num ".") num (str num "."))) [1 x l])))
         (sort-by (fn [[r _ l]] [r (label-key l)]))
         (take n)
         (mapv (fn [[_ x]]
                 (let [{:exercise/keys [concepts] :as e}
                       (d/pull db (conj (pop pull-spec) {:exercise/concepts [:concept/id :concept/label]}) x)]
                   (-> (tidy e)
                       (assoc :exercise/concepts (vec (sort-by :concept/label concepts)))
                       (dissoc :exercise/text :exercise/solution :exercise/points :exercise/refers))))))))
