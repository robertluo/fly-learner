(ns robertluo.fly-learner.learning-test
  "The learning stack without knowledge: a toy curriculum, the evidence
  store, the decaying model and quiz (model calls stubbed)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.learning.decay :as decay]
            [robertluo.fly-learner.learning.store :as store]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.quiz :as quiz]))

(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [conn (d/get-conn (str (System/getProperty "java.io.tmpdir") "/fly-learner-learning-" (random-uuid))
                           (merge store/schema quiz/schema))]
      (try (binding [*conn* conn] (t)) (finally (d/close conn))))))

(def toy
  "Any body of knowledge will do: here, two facts and how they connect."
  (reify learning/Curriculum
    (items [_] {:x 2 :y 1 [:x :y] 1})
    (prerequisites [_ item] (if (vector? item) [:x :y] []))
    (material [_ item] {:title (str item) :about (str "the toy item " item)
                        :sources [{:title "Toy" :text "x and y are toys."}]})))

(def day (* 24 60 60 1000))

(def with-exercise
  "The toy, with an exercise its author wrote on :x that tests :y too."
  (reify learning/Curriculum
    (items [_] (learning/items toy))
    (prerequisites [_ item] (learning/prerequisites toy item))
    (material [_ item]
      (cond-> (learning/material toy item)
        (= :x item) (assoc :exercises [{:id (java.util.UUID. 0 1) :title "Exercise 1.1" :prompt "Show x."
                                        :answer "x, shown." :points ["x"] :context "Exercise 1.0: x is a toy."
                                        :items [:y] :anchor {:page 3}}])))))

(defn- answer!
  "Grade `response` to `q` and record its evidence, as the app does."
  [q response]
  (let [graded (quiz/grade nil q response {})]
    (store/record! *conn* (quiz/evidence q graded (System/currentTimeMillis)))
    graded))

(deftest store-round-trip
  (store/record! *conn* {:item [:x :y] :kind :answer :grade 0.5 :at 1000 :source :q1})
  (store/record! *conn* [{:item :x :kind :seen :at 500}])
  (is (= [{:item :x :kind :seen :at 500}
          {:item [:x :y] :kind :answer :grade 0.5 :at 1000 :source :q1}]
         (store/evidence (d/db *conn*)))))

(deftest quiz-raises-the-score
  (let [model (decay/decaying (learning/mastery))
        now   (System/currentTimeMillis)
        score #(get (learning/scores model toy (learning/states model (store/evidence (d/db *conn*))) %) :x)]
    (with-redefs [llm/complete-json
                  (fn [_ {:keys [prompt schema]}]
                    (if (get-in schema [:properties :questions])
                      (do (is (re-find #"the toy item :x" prompt) "the material comes from the curriculum")
                          {:questions [{:prompt "What is x?" :answer "A toy." :points ["toy"]}]})
                      {:grade 0.9 :feedback "Right."}))]
      (let [[q] (quiz/generate nil toy :x {})]
        (quiz/save! *conn* [q])
        (is (= :x (:question/item (quiz/question (d/db *conn*) (:question/id q)))))
        (is (zero? (score now)) "nothing known yet")
        (is (= 0.9 (:grade (answer! q "A toy."))))
        (testing "a right answer raises the score; forgetting lowers it later"
          (let [after (score (System/currentTimeMillis))]
            (is (< 0.4 after))
            (is (< (score (+ now (* 30 day))) after))))
        (testing "the whole curriculum's score rose, and what to study next moved on"
          (let [scores (learning/scores model toy (learning/states model (store/evidence (d/db *conn*)))
                                        (System/currentTimeMillis))]
            (is (pos? (learning/overall toy scores)))
            ;; :x was worth 2.25 (weight 2, and half of [:x :y]'s 1 split between its two
            ;; prerequisites); half known, it is worth about half
            (is (< 1.1 (get (into {} (learning/next-items toy scores 3)) :x) 1.35))))))))

(deftest an-authors-exercise-counts-more
  (let [model (learning/mastery)
        score #(learning/scores model with-exercise (learning/states model (store/evidence (d/db *conn*))) 0)
        [q]   (quiz/authored with-exercise :x)]
    (is (= [:x :y] (:question/items q)) "every item it tests")
    (is (= :author (:question/by q)))
    (is (empty? (quiz/authored with-exercise :y)) "found from the item it is material of")
    (with-redefs [llm/complete-json
                  (fn [_ {:keys [prompt]}]
                    (is (re-find #"The author's solution: x, shown." prompt))
                    (is (re-find #"Exercise 1.0: x is a toy." prompt) "the exercises it builds on")
                    {:grade 1.0 :feedback "Right."})]
      (quiz/save! *conn* [q])
      (is (= q (dissoc (quiz/question (d/db *conn*) (:question/id q)) :question/at)) "stored whole")
      (answer! q "x.")
      (is (= {:x 0.75 :y 0.75} (select-keys (score) [:x :y])) "as two answers, on each item")
      (is (= {(:question/id q) 1.0} (quiz/attempts (d/db *conn*) (store/evidence (d/db *conn*))))))))
