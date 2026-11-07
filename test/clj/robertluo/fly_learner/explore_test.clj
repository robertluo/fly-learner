(ns robertluo.fly-learner.explore-test
  (:require [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.explore :as x]
            [robertluo.fly-learner.explore.examples :as examples]
            [robertluo.fly-learner.explore.knowledge :as xk]
            [robertluo.fly-learner.explore.store :as store]
            [robertluo.fly-learner.ingest :as ingest]))

(deftest examples-are-sound
  (doseq [[k spec] examples/all]
    (testing k
      (is (= [] (x/check (x/normalize spec)))))))

(defn- by-type [spec state t]
  (filter #(= t (:type %)) (x/view spec state)))

(deftest prisoners-dilemma
  (let [s  (x/normalize examples/prisoners-dilemma)
        st (x/init-state s)
        nash (fn [st] (for [[r row] (map-indexed vector (:cells (first (by-type s st :table))))
                            [c cell] (map-indexed vector row) :when (:highlight? cell)] [r c]))]
    (testing "T > R: only mutual defection"
      (is (= [[1 1]] (nash st))))
    (testing "T < R: mutual cooperation is an equilibrium too"
      (is (= [[0 0] [1 1]] (nash (x/act s st {:index 0 :value 2})))))
    (testing "clicking a cell picks it, clicking again lets go"
      (let [st (x/act s st {:index 2 :row 0 :col 1})]
        (is (= [0 1] (get st "pick")))
        (is (nil? (get (x/act s st {:index 2 :row 0 :col 1}) "pick")))))
    (testing "the button resets the sliders"
      (is (= 5 (get (x/act s (x/act s st {:index 0 :value 1}) {:index 5}) "t"))))))

(deftest steps-see-derived-values
  (let [s  (x/normalize examples/beta-reduction)
        applicative (x/act s (x/init-state s) {:index 0 :value 1})
        formula (fn [st] (first (by-type s st :formula)))]
    (is (= "z" (:text (formula (x/act s (x/init-state s) {:index 3 :step 2})))))
    (is (= "(λw. w w) (λw. w w)" (:mark (formula (x/act s applicative {:index 3 :step 2})))))
    (testing "the toggle hides the mark"
      (is (= "" (:mark (formula (x/act s applicative {:index 1 :value false}))))))))

(deftest predict-reveals-once
  (let [s  (x/normalize examples/mixed-strategy)
        st (x/act s (x/init-state s) {:index 0 :value 1})
        p  (first (by-type s st :predict))]
    (is (= [1 2 false] [(:chosen p) (:answer p) (:correct? p)]))
    (is (string? (:explain p)))
    (testing "a guess stays"
      (is (= 1 (get (x/act s st {:index 0 :value 2}) "guess"))))))

;;=============================================================================
;; Kept explorations over the knowledge (no model)
;;=============================================================================

(deftest kept-explorations-survive-re-ingest
  (let [conn (db/connect (str (System/getProperty "java.io.tmpdir") "/fly-learner-explore-" (random-uuid))
                         store/schema)
        path "data/sources/thesis.pdf"
        nid  (fn [title] (d/q '[:find ?id . :in $ ?t :where [?n :node/title ?t] [?n :node/id ?id]] (d/db conn) title))
        ingest! (fn [] (d/transact! conn [{:db/id "s" :source/id (random-uuid) :source/path path :source/title "Thesis"}
                                          {:db/id "g" :node/id (random-uuid) :node/source "s" :node/title "Reduction order"
                                           :node/kind :group :node/summary "Normal vs applicative order." :node/order 1
                                           :node/children ["p"]}
                                          {:db/id "p" :node/id (random-uuid) :node/source "s" :node/title "K z Ω"
                                           :node/kind :paragraph :node/text "K z Ω reduces to z." :node/order 2}]))]
    (try
      (ingest!)
      (testing "the material is the node and the text under it"
        (is (= [(nid "Reduction order") (nid "K z Ω")] (map :id (xk/material (d/db conn) (nid "Reduction order"))))))
      (testing "a node is judged once"
        (is (= [(nid "Reduction order")] (xk/candidates (d/db conn))))
        (store/save-proposals! conn [{:proposal/node (nid "Reduction order") :proposal/kind :steps
                                      :proposal/pitch "Step through K z Ω"}] "m")
        (is (= [] (xk/candidates (d/db conn)))))
      (let [db  (d/db conn)
            g   (nid "Reduction order")
            id  (store/save! conn {:exploration/title "Reduction order" :exploration/spec (x/normalize examples/beta-reduction)
                                   :exploration/anchor (xk/anchor db g) :exploration/cites [(xk/anchor db (nid "K z Ω"))]})]
        (is (= {:proposal {:kind :steps :pitch "Step through K z Ω"}
                :explorations [{:exploration/id id :exploration/title "Reduction order"}]}
               (update (xk/for-node (d/db conn) g) :explorations #(mapv (fn [e] (dissoc e :exploration/at)) %))))
        (is (= (x/normalize examples/beta-reduction) (:exploration/spec (store/by-id (d/db conn) id)))
            "the spec comes back as kept")
        ;; re-ingest: the old source and nodes go, new ones with new ids come
        (ingest/remove-source! conn path)
        (ingest!)
        (let [db (d/db conn) x (xk/exploration db id)]
          (is (= (nid "Reduction order") (get-in x [:exploration/node :node/id])) "found again by title")
          (is (= [(nid "K z Ω")] (map :node/id (:exploration/cites x))))
          (is (= [id] (map :exploration/id (:explorations (xk/for-node db (nid "Reduction order"))))))))
      (finally (d/close conn)))))
