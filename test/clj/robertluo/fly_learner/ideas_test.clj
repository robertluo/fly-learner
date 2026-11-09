(ns robertluo.fly-learner.ideas-test
  "Anchors over the knowledge survive knowledge changing under them."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datalevin.core :as d]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ideas.knowledge :as anchors]
            [robertluo.fly-learner.ideas.store :as store]))

(def ^:dynamic *conn* nil)

(def sid #uuid "00000000-0000-0000-0000-0000000000a1")
(def para "Confluence is proved by the standard parallel reduction method.")

(use-fixtures :each
  (fn [t]
    (let [conn (db/connect (str (System/getProperty "java.io.tmpdir") "/fly-learner-ideas-" (random-uuid))
                           store/schema)]
      (try
        (d/transact! conn [{:db/id "s" :source/id sid :source/title "Thesis"}
                           {:node/id (random-uuid) :node/source "s" :node/title "2.2 Confluence"
                            :node/kind :section :node/page 28}
                           {:node/id (random-uuid) :node/source "s" :node/title "Confluence is..."
                            :node/kind :paragraph :node/page 28 :node/text para}
                           {:concept/id (random-uuid) :concept/label "confluence" :concept/aliases ["confluence"]}
                           {:concept/id (random-uuid) :concept/label "church-rosser" :concept/aliases ["church-rosser"]}])
        (binding [*conn* conn] (t))
        (finally (d/close conn))))))

(defn- cid [label] (d/q '[:find ?id . :in $ ?l :where [?c :concept/label ?l] [?c :concept/id ?id]] (d/db *conn*) label))
(defn- nid [title] (d/q '[:find ?id . :in $ ?t :where [?n :node/title ?t] [?n :node/id ?id]] (d/db *conn*) title))
(defn- target [anchor] (ideas/target (anchors/anchors (d/db *conn*)) anchor))

(deftest concepts-follow-merges
  (let [a {:anchor/kind :concept :concept/id (cid "church-rosser")}
        r {:anchor/kind :relation :concept/ids [(cid "church-rosser") (cid "confluence")]}]
    (is (= [:concept (cid "church-rosser")] (target a)))
    (concept/merge! *conn* (cid "confluence") (cid "church-rosser"))
    (is (= [:concept (cid "confluence")] (target a)) "an idea on a merged concept moves with it")
    (is (nil? (target r)) "a relation whose two ends became one is gone")))

(deftest nodes-and-quotes-survive-re-ingest
  (let [n {:anchor/kind :node :node/id (nid "2.2 Confluence") :source/id sid :node/title "2.2 Confluence"}
        i (.indexOf ^String para "parallel reduction")
        q (merge {:anchor/kind :quote :source/id sid :field :text :node/id (nid "Confluence is...")}
                 (ideas/quote-selector para i (+ i 18)))]
    (is (= [:node (nid "Confluence is...")] (target q)))
    ;; re-ingest: same content, new node ids, the paragraph's line breaks changed
    (d/transact! *conn* [[:db/retractEntity [:node/id (nid "2.2 Confluence")]]
                         [:db/retractEntity [:node/id (nid "Confluence is...")]]])
    (d/transact! *conn* [{:node/id (random-uuid) :node/source [:source/id sid] :node/title "2.2 Confluence"
                          :node/kind :section :node/page 28}
                         {:node/id (random-uuid) :node/source [:source/id sid] :node/title "Confluence is..."
                          :node/kind :paragraph :node/page 28
                          :node/text "Confluence is proved by the standard parallel\nreduction method."}])
    (is (= [:node (nid "2.2 Confluence")] (target n)) "found again by title")
    (is (= [:node (nid "Confluence is...")] (target q)) "found again by its text")))

(deftest pdf-quotes
  (let [pdf (fn [text page] (merge {:anchor/kind :quote :source/id sid :field :page :page page}
                                   (ideas/quote-selector text 0 (count text))))]
    (is (= [:node (nid "Confluence is...")] (target (pdf "standard parallel reduction" 29)))
        "a paragraph on the page or next to it")
    (is (= [:page sid 28] (target (pdf "a formula pdf.js reads differently" 28)))
        "no paragraph has it: the idea stays on its page")))
