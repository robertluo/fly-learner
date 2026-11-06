(ns learner.knowledge-test
  (:require [cljs.test :refer [deftest is]]
            [learner.knowledge :as k]))

(deftest leaf
  (is (k/leaf? {:node/children []}))
  (is (not (k/leaf? {:node/children [(random-uuid)]}))))
