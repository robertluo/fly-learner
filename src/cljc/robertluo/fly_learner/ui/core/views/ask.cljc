(ns robertluo.fly-learner.ui.core.views.ask
  "Questions to the knowledge base: the answer with its citations, the cited
  subgraph, and the matches found at once."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.node :as node-ui]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(defn answer-parts
  "Split answer text into [:text s] and [:cite n] parts."
  [s]
  (for [tok (re-seq #"\[\d+\]|[^\[]+|\[" s)]
    (if-let [[_ n] (re-matches #"\[(\d+)\]" tok)]
      [:cite (parse-long n)]
      [:text tok])))

(defn answer-view [{:ask/keys [answer cited]} dispatch!]
  [:div.answer
   (for [[i para] (map-indexed vector (str/split answer #"\n\s*\n"))]
     [:p {:replicant/key i}
      (for [[j [kind v]] (map-indexed vector (answer-parts para))]
        (if (= :cite kind)
          (if-let [n (get cited (dec v))]
            (nav/node-link dispatch! n {:class "cite" :title (:node/title n) :replicant/key j} (str v))
            [:span.cite.gone {:replicant/key j :title "No longer in the knowledge base"} (str v)])
          [:span {:replicant/key j} v]))])])

(defn- subgraph-tree
  "The cited nodes with the paths that lead to them, as a nested outline."
  [dispatch! nodes]
  [:ul.tree
   (for [{:node/keys [id kind title summary text] :keys [cite children] :as n} nodes]
     [:li {:replicant/key id :class (when cite "cited")}
      [:div.tree-row
       (when cite [:span.cite-num (str cite)])
       (nav/node-link dispatch! n {:class "tree-title"}
                      (if (= :paragraph kind) (t/snippet text 90) title))
       (node-ui/meta-line n)]
      (when cite
        (if (= :paragraph kind)
          [:p.source-text (t/snippet text 600)]
          (when summary [:p.preview summary])))
      (when (seq children) (subgraph-tree dispatch! children))])])

(defn ask-view [{:keys [asking search-hits loading?] :as db} dispatch!]
  (let [ask      (db/current-ask db)
        question (or (:ask/question ask) asking)]
    [:main.single.ask
     (when-let [q (:ask/quote ask)]
       [:blockquote.asked-about {:replicant/key "quote"} "“" (t/snippet q 400) "”"])
     [:span.meta (if (:ask/quote ask) "Question about the passage" "Question")]
     [:h1 question]
     (cond
       ask
       (list
        (answer-view ask dispatch!)
        (when (seq (:ask/followups ask))
          [:div.followups {:replicant/key "followups"}
           (for [q (:ask/followups ask)]
             [:button.chip {:replicant/key q :on {:click #(dispatch! (nav/ask-effects q))}} q])])
        (when (seq (:ask/tree ask))
          [:section {:replicant/key "tree"}
           [:h2 "In the knowledge base"]
           (subgraph-tree dispatch! (:ask/tree ask))])
        (when (seq (:ask/matches ask))
          [:section {:replicant/key "matches"}
           [:h2 "Also matched"]
           (node-ui/hit-cards dispatch! (:ask/matches ask))]))

       asking
       (list
        [:p.muted.thinking {:replicant/key "thinking"} "Answering from the knowledge base"]
        (when-let [hits (seq (get search-hits asking))]
          [:section {:replicant/key "hits"}
           [:h2 "Matches"]
           (node-ui/hit-cards dispatch! hits)]))

       loading? [:p.muted "Loading…"]
       :else    [:p.muted "Not found."])]))

^:rct/test
(comment
  (answer-parts "A [1]. B [2][10] [x] end")
  ;=> [[:text "A "] [:cite 1] [:text ". B "] [:cite 2] [:cite 10] [:text " "] [:text "["] [:text "x] end"]]
  )

^:rct/test
(comment
  (answer-parts "A [1]. B [2][10] [x] end")
  ;=> [[:text "A "] [:cite 1] [:text ". B "] [:cite 2] [:cite 10] [:text " "] [:text "["] [:text "x] end"]]
  )
