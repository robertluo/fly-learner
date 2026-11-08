(ns robertluo.fly-learner.exercises-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.exercises :as exercises]
            [robertluo.fly-learner.llm :as llm]))

(defn- book
  "A source: one chapter teaching dominance, its exercises and solutions."
  [conn s dom eq]
  (let [para (fn [i page text] {:db/id (str "p" i) :node/id (random-uuid) :node/kind :paragraph :node/level 3
                                :node/order i :node/page page :node/text text :node/source "s"})]
    (d/transact! conn [{:db/id "s" :source/id s :source/path "data/sources/book.pdf" :source/root "root"}
                       {:concept/id dom :concept/label "strict dominance" :concept/aliases ["strict dominance"]}
                       {:concept/id eq :concept/label "dominant equilibrium" :concept/aliases ["dominant equilibrium"]}
                       {:db/id "root" :node/id (random-uuid) :node/kind :source :node/level 0 :node/source "s"
                        :node/children ["ch"]}
                       {:db/id "ch" :node/id (random-uuid) :node/kind :section :node/level 1 :node/order 0
                        :node/source "s" :node/children ["p0" "p1" "p2" "p3" "p4"]}
                       (assoc (para 0 1 "A strategy is strictly dominated when ..") :node/keywords ["strict dominance"])
                       (assoc (para 1 2 "Exercise 1.1. Find the dominated strategies.") :node/keywords ["dominant equilibrium"])
                       (para 2 2 "Exercise 1.2. Use Exercise 1.1 to find the equilibrium.")
                       (para 3 3 "Appendix 1.S: Solutions to exercises")
                       (para 4 3 "Exercise 1.1. Strategy B is dominated by A.")])))

(deftest found-linked-stored
  (let [conn      (db/connect (str (System/getProperty "java.io.tmpdir") "/fly-learner-exercises-" (random-uuid)))
        [s dom eq] (repeatedly 3 random-uuid)
        prompts   (atom [])]
    (try
      (book conn s dom eq)
      (with-redefs [llm/complete-json
                    (fn [_ {:keys [prompt]}]
                      (swap! prompts conj prompt)          ; checked outside: this runs on a pool thread
                      {:exercises [{:n 1 :concepts ["Strict Dominance" "not a concept"] :summary "Find them"
                                    :points ["B is dominated"]}
                                   {:n 2 :concepts ["dominant equilibrium" "strict dominance"] :summary "Equilibrium"
                                    :points ["(A, A)"]}]})]
        (is (= {:sources 1 :exercises 2 :solutions 1} (exercises/extract! conn nil {})))
        (is (empty? (exercises/pending (d/db conn))) "looked at once")
        (is (str/includes? (first @prompts) "- strict dominance\n") "the chapter's concepts")
        (is (str/includes? (first @prompts) "Strategy B is dominated by A.") "the author's solution")
        (is (str/includes? (first @prompts) "(It builds on: Exercise 1.1. Find") "the exercise it builds on")
        (let [[e1 e2] (exercises/of-concept (d/db conn) dom)]
          (is (= ["1.1" "1.2"] (map :exercise/label [e1 e2])))
          (is (= [dom] (:exercise/concepts e1)) "unknown labels dropped")
          (is (= "Exercise 1.1. Strategy B is dominated by A." (:exercise/solution e1)))
          (is (nil? (:exercise/solution e2)))
          (is (= ["1.1"] (:exercise/refers e2)))
          (is (= 2 (:exercise/page e2)))
          (is (= (exercises/exercise-id s "1.1") (:exercise/id e1)) "found again, the same id")
          (exercises/extract! conn nil {:source s})
          (is (= 2 (count @prompts)) "one call a chapter's batch")
          (is (= (:exercise/id e1) (:exercise/id (first (exercises/of-concept (d/db conn) dom)))))
          (is (= 2 (count (d/q '[:find [?x ...] :where [?x :exercise/id]] (d/db conn)))) "no duplicates"))
        (is (= ["1.2"] (map :exercise/label (exercises/of-concept (d/db conn) eq))))
        (concept/merge! conn dom eq)
        (is (= [["1.1" [dom]] ["1.2" [dom]]]
               (map (juxt :exercise/label :exercise/concepts) (exercises/of-concept (d/db conn) dom)))
            "a merged concept's exercises are the survivor's"))
      (finally (d/close conn)))))
