(ns robertluo.fly-learner.db-test
  (:require [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [robertluo.fly-learner.db :as db]))

(deftest node-beside-a-page
  ;; two passages: A on pages 10-11 (its last paragraph runs over into 12), B from 12;
  ;; page 13 is a figure, no paragraph
  (let [conn (db/connect (str (System/getProperty "java.io.tmpdir") "/fly-learner-db-" (random-uuid)))
        sid  (random-uuid)
        id   (fn [t] (d/q '[:find ?id . :in $ ?t :where [?n :node/title ?t] [?n :node/id ?id]] (d/db conn) t))
        para (fn [t o p] {:db/id t :node/id (random-uuid) :node/source "s" :node/kind :paragraph :node/level 3
                          :node/title t :node/order o :node/page p})]
    (try
      (d/transact! conn [{:db/id "s" :source/id sid :source/title "Book"}
                         {:db/id "A" :node/id (random-uuid) :node/source "s" :node/kind :group :node/level 2
                          :node/title "A" :node/order 1 :node/page 10 :node/children ["a1" "a2"]}
                         (para "a1" 2 10) (para "a2" 3 11)
                         {:db/id "B" :node/id (random-uuid) :node/source "s" :node/kind :group :node/level 2
                          :node/title "B" :node/order 4 :node/page 12 :node/children ["b1" "b2"]}
                         (para "b1" 5 12) (para "b2" 6 14)])
      (let [at (fn [p near] (db/node-at-page (d/db conn) sid p near))]
        (testing "the node shown stays while its text is on the page"
          (is (= (id "A") (at 11 (id "A"))))
          (is (= (id "A") (at 12 (id "A"))) "its last paragraph may run over"))
        (testing "else the node of the same kind and level on the page"
          (is (= (id "B") (at 14 (id "A"))))
          (is (= (id "B") (at 13 (id "A"))) "a page without paragraphs counts as the one before")
          (is (= (id "A") (at 10 (id "B")))))
        (testing "a paragraph is followed by a paragraph"
          (is (= (id "b2") (at 14 (id "a1"))))))
      (finally (d/close conn)))))
