(ns robertluo.fly-learner.quiz
  "Quizzes: questions that test an item of learning, and free-text answers
  graded into evidence.

  Built only on robertluo.fly-learner.learning: the item and its material
  come from a Curriculum, and the outcome is an :answer evidence for the
  log. Knows nothing of concepts, passages or how scores are computed.
  `schema` (questions) is merged into the database's at the composition root."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.learning.store :as store]
            [robertluo.fly-learner.llm :as llm]))

(def schema
  {:question/id     {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :question/item   {}                                 ; the learning item it tests (EDN)
   :question/prompt {:db/valueType :db.type/string}
   :question/answer {:db/valueType :db.type/string}   ; model answer
   :question/points {}                                 ; [key point ..] a full answer covers
   :question/at     {:db/valueType :db.type/instant}})

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
  grading checks the points, not the wording.")

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

(defn save! [conn questions]
  (d/transact! conn (mapv #(assoc % :question/at (java.util.Date.)) questions))
  nil)

(defn question [db id]
  (when (d/entid db [:question/id id])
    (d/pull db [:question/id :question/item :question/prompt :question/answer :question/points] [:question/id id])))

;;=============================================================================
;; Grading
;;=============================================================================

(def ^:private grade-system
  "You grade a learner's free-text answer to a study question, against a model
answer and the key points a full answer covers.

- grade: from 0 to 1, the share of the key points the answer gets right in
  substance, in any wording. Wrong statements lower it. A blank, off-topic
  or \"I don't know\" answer is 0.
- feedback: two or three sentences to the learner: what was right, what was
  missing or wrong, and the idea to remember. Address them as \"you\".")

(def ^:private grade-schema
  {:type "object"
   :properties {:grade {:type "number"} :feedback {:type "string"}}
   :required ["grade" "feedback"]
   :additionalProperties false})

(defn grade
  "Grade `response` to `question`: {:grade 0..1 :feedback str}."
  [client {:question/keys [prompt answer points]} response {:keys [models]}]
  (-> (llm/complete-json client {:system grade-system
                                 :prompt (str "Question: " prompt
                                              "\n\nModel answer: " answer
                                              "\n\nKey points:\n" (str/join "\n" (map #(str "- " %) points))
                                              "\n\nLearner's answer: " (if (str/blank? response) "(blank)" response))
                                 :model  (llm/model-for models :strong)
                                 :schema grade-schema})
      (update :grade #(-> % double (max 0.0) (min 1.0)))))

(defn evidence
  "The evidence a graded answer gives about the question's item."
  [question {:keys [grade]} at]
  {:item (:question/item question) :kind :answer :grade grade :at at :source (:question/id question)})

(defn answer!
  "Grade `response` to `question` and record it as evidence. Returns the grading."
  [conn client question response opts]
  (let [graded (grade client question response opts)]
    (store/record! conn (evidence question graded (System/currentTimeMillis)))
    graded))
