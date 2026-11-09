(ns robertluo.fly-learner.quiz
  "Quizzes: questions that test an item of learning, and free-text answers
  graded into evidence.

  Built only on robertluo.fly-learner.learning: the item and its material
  come from a Curriculum, and the outcome is an :answer evidence for the
  log (the caller records it). Questions are written by the model (`generate`) or by the sources'
  authors (`authored`: the material's :exercises, graded against the
  author's answer); an author's counts `authored-weight` times, for each
  item it tests. Knows nothing of concepts, passages or how scores are computed.
  `schema` (questions) is merged into the database's at the composition root."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.llm :as llm]))

(def schema
  {:question/id     {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :question/item   {}                                 ; the learning item it tests (EDN)
   :question/prompt {:db/valueType :db.type/string}
   :question/answer {:db/valueType :db.type/string}   ; model answer
   :question/points {}                                 ; [key point ..] a full answer covers
   :question/at     {:db/valueType :db.type/instant}
   ;; written by the sources' author (`authored`)
   :question/by      {:db/valueType :db.type/keyword} ; :author (absent: the model)
   :question/title   {:db/valueType :db.type/string}  ; e.g. "Exercise 1.5"
   :question/items   {}                                ; [item ..] every item it tests
   :question/weight  {:db/valueType :db.type/double}  ; how much an answer counts
   :question/context {:db/valueType :db.type/string}  ; text the grader needs besides the prompt
   :question/anchor  {}                                ; where it is (opaque)
   :question/verbatim? {:db/valueType :db.type/boolean}}) ; plain text as found, not Markdown

(defn- render-material
  [{:keys [about sources]}]
  (str "The learner should know " about ".\n\nSources:\n\n"
       (str/join "\n\n" (map-indexed (fn [i {:keys [title text]}] (str "[" (inc i) "] " title "\n" text))
                                     sources))))

;;=============================================================================
;; Questions
;;=============================================================================

(def ^:private ask-system
  "You are a tutor writing questions that test whether a learner really knows
something, not whether they recognise wording.

- Ask the learner to explain, apply or connect ideas; avoid yes/no and pure
  recall of definitions, unless the item is a definition to remember.
- Each question must be answerable from the sources alone, in a few sentences.
- Keep the sources' notation and terms. Do not mention \"the sources\" or
  section numbers in the question.
- Give a model answer, and the key points (2-4, short) a full answer covers;
  grading checks the points, not the wording.
- Write the question, the model answer and the points in Markdown: a table
  for a game's payoffs, a matrix or anything in rows and columns; lists for
  steps; TeX for formulas, $..$ inline and $$..$$ displayed; a dollar sign
  that is money as \\$. No headings.")

(def ^:private questions-schema
  {:type "object"
   :properties {:questions {:type "array"
                            :items {:type "object"
                                    :properties {:prompt {:type "string"}
                                                 :answer {:type "string"}
                                                 :points {:type "array" :items {:type "string"}}}
                                    :required ["prompt" "answer" "points"]
                                    :additionalProperties false}}}
   :required ["questions"]
   :additionalProperties false})

(defn generate
  "`n` (default 1) questions testing `item` of `curriculum`."
  [client curriculum item {:keys [n models] :or {n 1}}]
  (let [material (learning/material curriculum item)]
    (->> (llm/complete-json client {:system ask-system
                                    :prompt (str (render-material material) "\n\nWrite " n " question(s).")
                                    :model  (llm/model-for models :strong)
                                    :schema questions-schema})
         :questions
         (take n)
         (mapv (fn [{:keys [prompt answer points]}]
                 {:question/id     (random-uuid)
                  :question/item   item
                  :question/prompt prompt
                  :question/answer answer
                  :question/points (vec points)})))))

(def authored-weight
  "An answer to a question the sources' author wrote counts this many times:
  the author knows what matters, and grades against their own solution."
  2.0)

(defn authored
  "The questions the sources' authors wrote on `item` of `curriculum` (its
  material's :exercises), in their order."
  [curriculum item]
  (vec (for [{:keys [id title prompt answer points context items anchor verbatim?]} (:exercises (learning/material curriculum item))]
         (cond-> {:question/id     id
                  :question/item   item
                  :question/items  (vec (distinct (cons item items)))
                  :question/prompt prompt
                  :question/answer (or answer "")
                  :question/points (vec points)
                  :question/by     :author
                  :question/title  title
                  :question/weight authored-weight}
           context   (assoc :question/context context)
           anchor    (assoc :question/anchor anchor)
           verbatim? (assoc :question/verbatim? true)))))

(defn save! [conn questions]
  (d/transact! conn (mapv #(assoc % :question/at (java.util.Date.)) questions))
  nil)

(defn question [db id]
  (when (d/entid db [:question/id id])
    (d/pull db [:question/id :question/item :question/prompt :question/answer :question/points :question/by
                :question/title :question/items :question/weight :question/context :question/anchor]
            [:question/id id])))

(defn attempts
  "{question-id best grade} of the authored questions answered, from the
  `evidence` log (see robertluo.fly-learner.learning)."
  [db evidence]
  (let [authored (set (d/q '[:find [?id ...] :where [?q :question/by :author] [?q :question/id ?id]] db))]
    (reduce (fn [m {:keys [source grade]}]
              (if (and grade (contains? authored source)) (update m source (fnil max 0.0) grade) m))
            {} evidence)))

;;=============================================================================
;; Grading
;;=============================================================================

(def ^:private grade-system
  "You grade a learner's free-text answer to a study question, against a model
answer and the key points a full answer covers.

- grade: from 0 to 1, the share of the key points the answer gets right in
  substance, in any wording. Wrong statements lower it. A blank, off-topic
  or \"I don't know\" answer is 0.
- A question from a book comes with the author's solution, transcribed into
  Markdown or as text taken from the book's file (a PDF, an EPUB), where tables and formulas may be
  flattened into runs of numbers. Read them as best you can; a learner may
  describe a table in words or write formulas in TeX.
- feedback: two or three sentences to the learner: what was right, what was
  missing or wrong, and the idea to remember. Address them as \"you\".
  Markdown, TeX for formulas ($..$); a small table only when it shows the
  point best.")

(def ^:private grade-schema
  {:type "object"
   :properties {:grade {:type "number"} :feedback {:type "string"}}
   :required ["grade" "feedback"]
   :additionalProperties false})

(defn grade
  "Grade `response` to `question`: {:grade 0..1 :feedback str}."
  [client {:question/keys [prompt answer points by context]} response {:keys [models]}]
  (-> (llm/complete-json client {:system grade-system
                                 :prompt (str (when context (str "It builds on:\n" context "\n\n"))
                                              "Question: " prompt
                                              (if (= :author by) "\n\nThe author's solution: " "\n\nModel answer: ")
                                              answer
                                              "\n\nKey points:\n" (str/join "\n" (map #(str "- " %) points))
                                              "\n\nLearner's answer: " (if (str/blank? response) "(blank)" response))
                                 :model  (llm/model-for models :strong)
                                 :schema grade-schema})
      (update :grade #(-> % double (max 0.0) (min 1.0)))))

(defn evidence
  "The evidence a graded answer gives about each item the question tests,
  weighted by the question's weight."
  [{:question/keys [item items weight id]} {:keys [grade]} at]
  (vec (for [i (distinct (cons item items))]
         (cond-> {:item i :kind :answer :grade grade :at at :source id}
           weight (assoc :weight weight)))))
