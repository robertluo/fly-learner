(ns learner.quiz
  "Generate questions for any node of the DAG on demand, to test real
  understanding instead of the illusion of competence."
  (:require [learner.llm :as llm]))

(def ^:private system-prompt
  "You are a tutor writing test questions. For :understand, ask questions
that require explaining, applying or connecting the ideas, not recalling
wording. For :memorize, ask short recall questions (vocabulary, grammar,
definitions, facts). Give each question a model answer.")

(def ^:private questions-schema
  {:type "object"
   :properties {:questions {:type "array"
                            :items {:type "object"
                                    :properties {:prompt {:type "string"} :answer {:type "string"}}
                                    :required ["prompt" "answer"]
                                    :additionalProperties false}}}
   :required ["questions"]
   :additionalProperties false})

(defn generate
  "Return question maps for `node` (a pulled node with :node/summary or :node/text)."
  [client node {:keys [kind n models] :or {kind :understand n 3}}]
  (let [content (or (:node/summary node) (:node/text node))
        prompt  (str "Kind: " kind "\nCount: " n "\n\nMaterial:\n" content)]
    (->> (:questions (llm/complete-json client {:system system-prompt :prompt prompt
                                                :model (llm/model-for models :strong)
                                                :schema questions-schema}))
         (mapv (fn [{:keys [prompt answer]}]
                 {:question/id     (random-uuid)
                  :question/node   [:node/id (:node/id node)]
                  :question/kind   kind
                  :question/prompt prompt
                  :question/answer answer})))))
