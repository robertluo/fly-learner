(ns robertluo.fly-learner.server.study
  "Studying, composed for the API: learning (with the model and curriculum
  picked in robertluo.fly-learner.server.system), its evidence store, and
  quiz. App layer: the only place these meet.

  `learning` below is {:model Model :curriculum (fn [db] Curriculum)}."
  (:require [datalevin.core :as d]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.learning.store :as store]
            [robertluo.fly-learner.quiz :as quiz]))

(def next-limit 6)

(defn- now [] (System/currentTimeMillis))

(defn- scored
  "Curriculum and scores for `db` at `at`."
  [db {:keys [model curriculum]} at]
  (let [c (curriculum db)]
    [c (learning/scores model c (learning/states model (store/evidence db)) at)]))

(defn- item-ref [c scores item]
  (let [{:keys [title about]} (learning/material c item)]
    {:item item :title title :about about :score (get scores item 0)}))

(defn overview
  "What the learner knows now: {:overall :scores {item score} (only items
  above 0) :next [{:item :title :about :score :priority} ..] :answers n}."
  [db learning]
  (let [[c scores] (scored db learning (now))]
    {:overall (learning/overall c scores)
     :scores  (into {} (filter (comp pos? val)) scores)
     :next    (vec (for [[item p] (learning/next-items c scores next-limit)]
                     (assoc (item-ref c scores item) :priority p)))
     :answers (count (filter #(= :answer (:kind %)) (store/evidence db)))}))

(defn- public-question
  "A question as the learner sees it before answering: no model answer."
  [c scores q]
  (merge (select-keys q [:question/id :question/item :question/prompt])
         (item-ref c scores (:question/item q))))

(defn new-question!
  "Write and save a question on `item`, or on the item to study next when
  nil. Returns it without its answer, or {:error ..}."
  [conn client learning item opts]
  (let [db (d/db conn)
        [c scores] (scored db learning (now))
        item (or item (ffirst (learning/next-items c scores 1)))]
    (if-not (contains? (learning/items c) item)
      {:error {:type :invalid :message "Nothing to study there"}}
      (let [[q] (quiz/generate client c item opts)]
        (quiz/save! conn [q])
        (public-question c scores q)))))

(defn answer!
  "Grade `response` to question `id`, record the evidence, and return the
  grading with the model answer, key points and the score of the item and
  of the whole curriculum before and after."
  [conn client learning id response opts]
  (if-let [q (quiz/question (d/db conn) id)]
    (let [item   (:question/item q)
          [c s0] (scored (d/db conn) learning (now))
          graded (quiz/answer! conn client q response opts)
          db     (d/db conn)
          [_ s1] (scored db learning (now))]
      (merge graded
             (select-keys q [:question/id :question/answer :question/points])
             {:before   {:item (get s0 item 0) :overall (learning/overall c s0)}
              :after    {:item (get s1 item 0) :overall (learning/overall c s1)}
              :overview (overview db learning)}))
    {:error {:type :not-found :message "No such question"}}))
