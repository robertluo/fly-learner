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
        (is (= 0.9 (:grade (quiz/answer! *conn* nil q "A toy." {}))))
        (testing "a right answer raises the score; forgetting lowers it later"
          (let [after (score (System/currentTimeMillis))]
            (is (< 0.4 after))
            (is (< (score (+ now (* 30 day))) after))))
        (testing "the whole curriculum's score rose, and what to study next moved on"
          (let [scores (learning/scores model toy (learning/states model (store/evidence (d/db *conn*)))
                                        (System/currentTimeMillis))]
            (is (pos? (learning/overall toy scores)))
            ;; :x was worth 2 (weight 2, unknown); half known, it is worth about half
            (is (< 1.0 (get (into {} (learning/next-items toy scores 3)) :x) 1.2))))))))
