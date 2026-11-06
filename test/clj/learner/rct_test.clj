(ns learner.rct-test
  "Runs the Rich Comment Tests (^:rct/test comment blocks) in src/ under Kaocha."
  (:require [clojure.test :refer [deftest]]
            [com.mjdowney.rich-comment-tests.test-runner :as rct]))

(deftest rct-tests
  (rct/run-tests-in-file-tree! :dirs #{"src"}))
