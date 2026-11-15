(ns robertluo.fly-learner.concept.prereq-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.concept.prereq :as prereq]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.llm :as llm]))

(deftest stored-judgement
  (let [dir     (str (System/getProperty "java.io.tmpdir") "/fly-learner-prereq-" (random-uuid))
        conn    (db/connect dir)
        [a b c] (repeatedly 3 random-uuid)
        s       (random-uuid)]
    (try
      (d/transact! conn [{:db/id "s" :source/id s}
                         {:concept/id a :concept/label "strategy" :concept/aliases ["strategy"]}
                         {:concept/id b :concept/label "mixed strategy" :concept/aliases ["mixed strategy"]}
                         {:concept/id c :concept/label "support" :concept/aliases ["support"]}
                         {:node/id (random-uuid) :node/source "s" :node/page 3 :node/keywords ["strategy"]}
                         {:node/id (random-uuid) :node/source "s" :node/page 9 :node/keywords ["mixed strategy" "support"]}])
      (is (= a (first (prereq/pending (d/db conn) s))) "strategy first: introduced first")
      (is (= #{a b c} (set (prereq/pending (d/db conn) s))))
      (d/transact! conn (prereq/->tx {:by "stub" :judged [a b c]
                                      :claims [{:of b :needs a :why "a mix of strategies"}
                                               {:of c :needs b :why "of a mixed strategy"}]
                                      :hidden [{:label "probability distribution" :gloss "Weights summing to one."
                                                :needed-by [{:of b :why "the mix"}]}]}))
      (let [db     (d/db conn)
            hidden (d/q '[:find ?id . :where [?c :concept/origin :inferred] [?c :concept/id ?id]] db)
            dag    (prereq/dag (prereq/claims db) (prereq/first-pages db))]
        (is (empty? (prereq/pending db s)) "judged concepts are not judged again")
        (is (= #{a hidden} (dag b)))
        (is (= #{a hidden b} (set (prereq/before dag c))))
        (is (= b (last (prereq/before dag c))) "each after what it needs")
        (is (= [b] (prereq/before dag c #{a hidden} str)) "known ones left out"))
      (finally (d/close conn)))))

(deftest merge-keeps-claims
  (let [dir     (str (System/getProperty "java.io.tmpdir") "/fly-learner-prereq-" (random-uuid))
        conn    (db/connect dir)
        [a a' b] (repeatedly 3 random-uuid)]
    (try
      (d/transact! conn [{:concept/id a :concept/label "state space" :concept/aliases ["state space"]}
                         {:concept/id a' :concept/label "set of states" :concept/aliases ["set of states"]}
                         {:concept/id b :concept/label "event" :concept/aliases ["event"]}])
      (d/transact! conn (prereq/->tx {:by "stub" :judged [b a']
                                      :claims [{:of b :needs a' :why "a subset of it"}]}))
      (concept/merge! conn a a')
      (is (= [{:of b :needs a :why "a subset of it" :by "stub"}] (prereq/claims (d/db conn))))
      (finally (d/close conn)))))

(deftest curation
  (let [dir   (str (System/getProperty "java.io.tmpdir") "/fly-learner-prereq-" (random-uuid))
        conn  (db/connect dir)
        [a b] (repeatedly 2 random-uuid)]
    (try
      (d/transact! conn [{:concept/id a :concept/label "transitivity" :concept/aliases ["transitivity"]}
                         {:concept/id b :concept/label "equivalence relation" :concept/aliases ["equivalence relation"]}])
      (d/transact! conn (prereq/->tx {:by "stub" :judged [a] :claims [{:of a :needs b :why "backwards"}]}))
      (is (= 1 (prereq/unclaim! conn a b)))
      (prereq/claim! conn b a "one of its defining properties")
      (is (= [{:of b :needs a :why "one of its defining properties" :by "human"}] (prereq/claims (d/db conn))))
      (is (= [:prereqs :unclaim :claim] (map :edit/op (concept/edits (d/db conn)))))
      (finally (d/close conn)))))

(deftest judged-in-halves-when-cut
  (let [dir   (str (System/getProperty "java.io.tmpdir") "/fly-learner-prereq-" (random-uuid))
        conn  (db/connect dir)
        s     (random-uuid)
        calls (atom [])]
    (try
      (d/transact! conn [{:db/id "s" :source/id s}
                         {:concept/id (random-uuid) :concept/label "strategy" :concept/aliases ["strategy"]}
                         {:concept/id (random-uuid) :concept/label "mixed strategy" :concept/aliases ["mixed strategy"]}
                         {:concept/id (random-uuid) :concept/label "support" :concept/aliases ["support"]}
                         {:node/id (random-uuid) :node/source "s" :node/page 3 :node/keywords ["strategy"]}
                         {:node/id (random-uuid) :node/source "s" :node/page 9 :node/keywords ["mixed strategy" "support"]}])
      ;; a reply about more than one concept (a "### label" each) is cut at its length
      (with-redefs [llm/complete-json
                    (fn [_ {:keys [prompt]}]
                      (swap! calls conj prompt)
                      (let [asked (map second (re-seq #"(?m)^### (.+)$" prompt))]
                        (cond (str/starts-with? prompt "Background names")
                              {:groups [{:label "probability distribution" :gloss "Weights summing to one."
                                         :names ["probability distribution"] :existing ""}]}
                              (< 1 (count asked)) (throw (ex-info "Reply truncated at max-tokens" {}))
                              :else {:concepts [{:concept    (first asked)
                                                 :needs      []
                                                 :background [{:label "probability distribution" :gloss "" :why "weights"}]}]})))]
        (is (= {:judged 3 :claims 3 :hidden 1} (prereq/judge! conn :stub s {:batch 3 :threads 1}))))
      (is (= 6 (count @calls)) "3 + 2 cut, then each alone, then one consolidation")
      (is (empty? (prereq/pending (d/db conn) s)))
      (finally (d/close conn)))))
