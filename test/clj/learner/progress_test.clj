(ns learner.progress-test
  (:require [clojure.test :refer [deftest is]]
            [learner.progress :as progress])
  (:import (java.time Instant)))

(def now (Instant/parse "2026-01-01T00:00:00Z"))

(deftest intervals-grow-when-correct
  (let [steps (iterate #(progress/next-review % true now) progress/initial)]
    (is (= [1 3 8] (map :review/interval (take 3 (rest steps)))))))

(deftest wrong-answer-resets
  (let [r (progress/next-review {:review/ease 2.5 :review/interval 10} false now)]
    (is (= 1 (:review/interval r)))
    (is (< (:review/ease r) 2.5))))
