(ns robertluo.fly-learner.ingest-test
  "A source is its document, not where its file is."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.fold :as fold]
            [robertluo.fly-learner.ingest :as ingest]))

(defn- tmp [prefix] (str (System/getProperty "java.io.tmpdir") "/" prefix (random-uuid)))

(deftest a-file-is-known-by-its-content
  (let [a (io/file (tmp "fly-learner-a-")) b (io/file (tmp "fly-learner-b-"))]
    (try
      (spit a "the same bytes")
      (spit b "the same bytes")
      (is (= (ingest/file-hash a) (ingest/file-hash b)) "wherever it is")
      (is (re-matches #"[0-9a-f]{64}" (ingest/file-hash a)))
      (spit b "other bytes")
      (is (not= (ingest/file-hash a) (ingest/file-hash b)))
      (is (= (ingest/source-id "h") (ingest/source-id "h")) "a new source's id: from its content")
      (finally (.delete a) (.delete b)))))

(def ^:private text
  ["Two players choose at once, each knowing the other is rational."
   "A Nash equilibrium is a profile of strategies from which no player gains by deviating alone."
   "In the battle of the sexes there are two pure equilibria and one in mixed strategies."
   "Backward induction solves finite games of perfect information from their last moves."])

(deftest a-source-is-found-again
  (let [conn  (db/connect (tmp "fly-learner-ingest-"))
        sid   (random-uuid)
        gone  (tmp "fly-learner-gone-")
        file  (io/file (tmp "fly-learner-here-"))
        node  {:node/id (random-uuid) :node/level 0 :node/kind :source :node/title "Book" :node/order 0}
        paras (for [[i t] (map-indexed vector text)]
                {:node/id (random-uuid) :node/level 1 :node/kind :paragraph :node/title (str i) :node/order (inc i)
                 :node/text t})
        found #(select-keys (ingest/same-source (d/db conn) %) [:source/id :by :was])]
    (try
      (spit file "the old edition")
      (d/transact! conn (fold/->tx {:id sid :title "Book" :path (str file) :hash "h1"} (cons node paras)))
      (testing "moved or renamed: the same content"
        (is (= {:source/id sid :by :content} (found {:path "elsewhere/renamed.pdf" :hash "h1"}))))
      (testing "changed in place: the same path"
        (is (= {:source/id sid :by :path} (found {:path (str file) :hash "h2"}))))
      (testing "moved and changed: its old file is still there, so a new document"
        (is (= {} (found {:path "elsewhere/new.pdf" :hash "h2" :texts text}))))
      (d/transact! conn [{:source/id sid :source/path gone}])
      (testing "moved and changed: the old file gone, most of the text the same"
        (let [edited (assoc text 3 "Backward induction solves finite games of perfect information, from the end.")]
          (is (= {:source/id sid :by :text :was gone} (found {:path "elsewhere/new.pdf" :hash "h2" :texts edited})))))
      (testing "another document is a new one"
        (is (= {} (found {:path "elsewhere/other.pdf" :hash "h3" :texts ["Lambda calculus has one rule of computation."]}))))
      (testing "removing it keeps nothing of it"
        (ingest/remove-source! conn sid)
        (is (nil? (d/entid (d/db conn) [:source/id sid])))
        (is (nil? (d/entid (d/db conn) [:node/id (:node/id node)]))))
      (finally (d/close conn) (.delete file)))))

(deftest shared-text
  (is (== 1.0 (ingest/overlap (ingest/shingles text) (ingest/shingles (take 2 text)))) "an extended edition")
  (is (< 0.6 (ingest/overlap (ingest/shingles text) (ingest/shingles (assoc text 0 "Each player moves without seeing what the other one chose."))) 1.0)
      "a paragraph rewritten")
  (is (== 0.0 (ingest/overlap #{} (ingest/shingles text)))))
