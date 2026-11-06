(ns robertluo.fly-learner.concept-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.concept.graph :as graph]
            [robertluo.fly-learner.db :as db]))

(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (System/getProperty "java.io.tmpdir") "/fly-learner-concept-" (random-uuid))
          conn (db/connect dir)]
      (try
        (d/transact! conn [{:node/id (random-uuid) :node/title "Equality" :node/level 1
                            :node/keywords ["equality type" "equality types" "proof"]}
                           {:node/id (random-uuid) :node/title "Untyped equality" :node/level 2
                            :node/keywords ["untyped equality" "equality types"]}
                           {:node/id (random-uuid) :node/title "Intersections" :node/level 1
                            :node/keywords ["dependent intersection" "intersection types"]}])
        (binding [*conn* conn] (t))
        (finally (d/close conn))))))

(defn- reply
  "Stub resolver: a fixed reply, whatever the batch."
  [r]
  (fn [_] (assoc r :by "stub")))

(def first-reply
  {:concepts [{:label "equality type" :gloss "Type of proofs that two terms are equal."
               :keywords ["equality type" "equality types"] :broader []}
              {:label "untyped equality" :gloss "Equality of erased terms."
               :keywords ["untyped equality" "equality types"] ; contested: the first concept keeps it
               :broader ["equality type" "no such concept"]}
              {:label "intersection type" :gloss "Type of terms having two types at once."
               :keywords ["intersection types"] :broader []}
              {:label "unused umbrella" :gloss "" :keywords [] :broader []}]
   :ignored ["proof"]})

(defn- by-label [db]
  (into {} (map (juxt :concept/label identity)) (concept/concepts db)))

(deftest resolve-and-curate
  (let [r (concept/resolve! *conn* (reply first-reply))
        cs (by-label (d/db *conn*))]
    (testing "resolution"
      (is (= {:batches 1 :new 3 :attached 0 :ignored 1 :missing #{"dependent intersection"}} r))
      (is (= #{"equality type" "equality types"} (get-in cs ["equality type" :concept/aliases])))
      (is (= #{(get-in cs ["equality type" :concept/id])} (get-in cs ["untyped equality" :concept/broader])))
      (is (= "stub" (get-in cs ["intersection type" :concept/by])))
      (is (= ["dependent intersection"] (keys (concept/unresolved (d/db *conn*))))))

    (testing "a second run sees existing concepts and only adds to them"
      (concept/resolve! *conn* (reply {:concepts [{:label "intersection type" :gloss "ignored"
                                                   :keywords ["dependent intersection"]
                                                   :broader []}]
                                       :ignored []}))
      (let [it (get (by-label (d/db *conn*)) "intersection type")]
        (is (= "Type of terms having two types at once." (:concept/gloss it)))
        (is (contains? (:concept/aliases it) "dependent intersection")))
      (is (empty? (concept/unresolved (d/db *conn*)))))

    (testing "the schema refuses a keyword naming two concepts"
      (is (thrown? Exception
                   (d/transact! *conn* [[:db/add [:concept/id (get-in cs ["untyped equality" :concept/id])]
                                         :concept/aliases "equality types"]]))))

    (let [id #(get-in (by-label (d/db *conn*)) [% :concept/id])]
      (testing "split a sense off, then merge it back"
        (let [new-id (concept/split! *conn* (id "intersection type") ["dependent intersection"]
                                     {:concept/label "dependent intersection" :concept/gloss "Second type depends on the first."})]
          (is (= #{"dependent intersection"} (:concept/aliases (get (by-label (d/db *conn*)) "dependent intersection"))))
          (concept/add-broader! *conn* new-id (id "intersection type"))
          (concept/merge! *conn* (id "intersection type") new-id)
          (let [cs (by-label (d/db *conn*))]
            (is (nil? (cs "dependent intersection")))
            (is (= #{"intersection types" "intersection type" "dependent intersection"}
                   (get-in cs ["intersection type" :concept/aliases])))
            (is (empty? (get-in cs ["intersection type" :concept/broader])) "no self loop after merge"))))

      (testing "merge moves children"
        (concept/merge! *conn* (id "intersection type") (id "equality type"))
        (is (= #{(id "intersection type")} (:concept/broader (get (by-label (d/db *conn*)) "untyped equality")))))

      (testing "assign, ignore, unignore"
        (concept/assign! *conn* "proof" (id "untyped equality"))
        (is (contains? (:concept/aliases (get (by-label (d/db *conn*)) "untyped equality")) "proof"))
        (is (nil? (d/entid (d/db *conn*) [:ignored/keyword "proof"])))
        (concept/ignore! *conn* "proof")
        (is (not (contains? (:concept/aliases (get (by-label (d/db *conn*)) "untyped equality")) "proof")))
        (concept/unignore! *conn* "proof")
        (is (= ["proof"] (keys (concept/unresolved (d/db *conn*))))))

      (testing "rename keeps the old name as an alias"
        (concept/rename! *conn* (id "untyped equality") "Untyped Equality")
        (let [cs (by-label (d/db *conn*))]
          (is (= #{"intersection type" "Untyped Equality"} (set (keys cs))))
          (is (contains? (get-in cs ["Untyped Equality" :concept/aliases]) "untyped equality"))))

      (testing "an edit failing in its second stage changes nothing"
        (let [before (concept/concepts (d/db *conn*))]
          ;; stage 1 takes "intersection types" off; stage 2 fails: the new
          ;; label is already another concept's alias
          (is (thrown? Exception (concept/split! *conn* (id "intersection type") ["intersection types"]
                                                 {:concept/label "untyped equality"})))
          (is (= before (concept/concepts (d/db *conn*))))
          (is (thrown? Exception (concept/merge! *conn* (id "intersection type") (id "intersection type")))))))

    (testing "every change is logged"
      (is (= [:resolve :resolve :split :add-broader :merge :merge :assign :ignore :unignore :rename]
             (mapv :edit/op (concept/edits (d/db *conn*)))))
      (is (= "human" (:edit/by (last (concept/edits (d/db *conn*)))))))))

(deftest graph-from-db
  (concept/resolve! *conn* (reply first-reply))
  (let [g   (graph/load-graph (d/db *conn*))
        cs  (by-label (d/db *conn*))
        id  #(get-in cs [% :concept/id])
        adj (graph/edges g)]
    (is (= 2 (count (get-in g [:mentions (id "equality type")]))) "\"equality types\" counts for it")
    (is (= :broader (get-in adj [(id "untyped equality") (id "equality type") :kind])))
    (is (= [(id "untyped equality") (id "equality type")]
           (mapv :concept (graph/path adj (id "untyped equality") (id "equality type")))))
    (is (nil? (graph/path adj (id "equality type") (id "intersection type"))))))
