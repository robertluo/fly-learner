(ns robertluo.fly-learner.ask-test
  (:require [clojure.test :refer [deftest is]]
            [datalevin.core :as d]
            [robertluo.fly-learner.ask :as ask]
            [robertluo.fly-learner.db :as db]))

(deftest a-numbered-label-comes-first
  (let [conn (db/connect (str (System/getProperty "java.io.tmpdir") "/fly-learner-ask-" (random-uuid)))
        para (fn [i page text] {:db/id (str "p" i) :node/id (random-uuid) :node/kind :paragraph :node/level 3
                                :node/order i :node/page page :node/text text :node/source "s"})
        ;; full-text sees "exercise 1 1": every paragraph here matches it about as well
        noise (for [i (range 10 40)]
                (para i (+ 60 i) (str "In exercise 1 of section 1, exercise " i " uses 1 and 1 again: exercise 1.")))]
    (try
      (d/transact! conn (concat
                         [{:db/id "s" :source/id (random-uuid) :source/root "root"}
                          {:db/id "root" :node/id (random-uuid) :node/kind :source :node/level 0 :node/source "s"
                           :node/children ["g"]}
                          {:db/id "g" :node/id (random-uuid) :node/kind :group :node/level 2 :node/title "Exercises"
                           :node/source "s" :node/children (mapv #(str "p" %) (concat (range 6) (range 10 40)))}
                          (para 0 50 "Exercise 1.10. Something else.")
                          (para 1 58 "Exercise 1.1. (a) The solution: a table.")
                          (para 2 50 "Exercise 1.1. Antonia and Bob cannot decide where to go to dinner.")
                          (para 3 50 "(a) Represent it as a table.")
                          (para 4 50 "(b) Find its reduced form.")
                          (para 5 52 "As in Exercise 1.1, the players move together.")]
                         noise))
      (let [texts (mapv :node/text (ask/search (d/db conn) "exercise 1.1" 6))]
        (is (= ["Exercise 1.1. Antonia and Bob cannot decide where to go to dinner."
                "Exercise 1.1. (a) The solution: a table."]
               (take 2 texts))
            "starting with it, the exercise before its solution")
        (is (= ["(a) Represent it as a table." "(b) Find its reduced form."] (take 2 (drop 2 texts)))
            "then what follows it")
        (is (not-any? #{"Exercise 1.10. Something else."} texts) "1.1 is not 1.10"))
      (finally (d/close conn)))))
