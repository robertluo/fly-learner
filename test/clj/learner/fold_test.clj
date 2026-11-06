(ns learner.fold-test
  (:require [clojure.test :refer [deftest is testing]]
            [learner.fold :as fold]))

(defn- fake-summarize [nodes]
  {:title    (str "group of " (count nodes))
   :summary  "summary"
   :keywords ["shared"]})

(def paragraphs
  (mapv #(hash-map :page 1 :order % :text (str "paragraph " %)) (range 13)))

(deftest fold-builds-layers
  (let [nodes  (fold/fold fake-summarize paragraphs {:group-size 3})
        levels (frequencies (map :node/level nodes))]
    (testing "single birdview root"
      (is (= 1 (levels 0))))
    (testing "leaves are the deepest level and keep source text"
      (let [deepest (apply max (keys levels))]
        (is (= 13 (levels deepest)))
        (is (every? :node/text (filter #(= deepest (:node/level %)) nodes)))))
    (testing "every child id exists"
      (let [ids (set (map :node/id nodes))]
        (is (every? ids (mapcat :node/children nodes)))))))

(deftest link-by-keywords-links-siblings
  (let [nodes (fold/link-by-keywords (fold/fold fake-summarize paragraphs {:group-size 3}))
        level-1 (filter #(= 1 (:node/level %)) nodes)]
    (is (every? #(seq (:node/links %)) level-1))))
