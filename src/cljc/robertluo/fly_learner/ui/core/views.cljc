(ns robertluo.fly-learner.ui.core.views
  "UI views: Replicant hiccup. Components receive db and dispatch!, and
  dispatch effect maps ({:db f :pull op :history :push})."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.history :as history]
            #?(:cljs [robertluo.fly-learner.ui.pdf :as pdf])))

;;=============================================================================
;; Helpers
;;=============================================================================

(def ^:private kind-label
  {:source "Overview" :section "Section" :group "Passage" :paragraph "Source text"})

(defn- link
  "An <a> with a real href (open in new tab works) whose plain click is
  handled in-app by dispatching `effects`."
  [dispatch! effects path attrs & children]
  (into [:a (merge {:href path
                    :on {:click (fn [e]
                                  (when-not (or (.-metaKey e) (.-ctrlKey e) (.-shiftKey e))
                                    (.preventDefault e)
                                    (dispatch! effects)))}}
                   attrs)]
        children))

(defn- open-node-effects [id]
  {:db #(db/open-node % id) :pull :node :history :push})

(defn- node-link
  ([dispatch! node] (node-link dispatch! node {} (:node/title node)))
  ([dispatch! {:node/keys [id]} attrs & children]
   (apply link dispatch! (open-node-effects id) (str "/n/" id) attrs children)))

(defn- keyword-link [dispatch! kw]
  (link dispatch! {:db #(db/open-keyword % kw) :pull :keyword :history :push}
        (history/state->path {:view :keyword :keyword kw})
        {:class "chip"}
        kw))

(defn- meta-line [{:node/keys [kind page]}]
  [:span.meta (kind-label kind) (when page (str " · p. " page))])

;;=============================================================================
;; Pieces
;;=============================================================================

(defn- ask-effects
  "Ask `q`: show it, search at once, answer with the model."
  [q]
  {:db #(db/ask-submitted % q) :pull [:search :ask-create]})

(defn- submit-ask
  "Form submit: ask the form's `q` input, then clear it."
  #?(:cljs [dispatch! e] :clj [_dispatch! _e])
  #?(:cljs (do (.preventDefault e)
               (let [input (.. e -target -elements -q)
                     q     (str/trim (.-value input))]
                 (when-not (str/blank? q)
                   (set! (.-value input) "")
                   (.blur input)
                   (dispatch! (ask-effects q)))))
     :clj  nil))

(defn- header [{:keys [asking]} dispatch!]
  [:header.top
   (link dispatch! {:db db/go-home :history :push} "/" {:class "brand"} "Fly Learner")
   [:form.ask {:on {:submit #(submit-ask dispatch! %)}}
    [:input {:name "q" :type "search" :autocomplete "off" :disabled (boolean asking)
             :placeholder (if asking "Answering…" "Ask the knowledge base…")}]]])

(defn- error-banner [{:keys [error]} dispatch!]
  (when error
    [:div.error
     [:span (or (:reason error) (name (:code error)))]
     [:button {:on {:click #(dispatch! {:db db/clear-error})}} "Dismiss"]]))

(defn- card
  "A node in a list: title link, kind/page, summary or source text."
  [dispatch! {:node/keys [id summary text kind] :as n}]
  [:li.card {:replicant/key id}
   (node-link dispatch! n {:class "card-title"}
              (if (= :paragraph kind) (meta-line n) (:node/title n)))
   (when-not (= :paragraph kind) (meta-line n))
   (cond
     (= :paragraph kind) [:p.source-text text]
     summary             [:p.preview summary])])

;;=============================================================================
;; Views
;;=============================================================================

(defn- ask-link [dispatch! {:ask/keys [id]} attrs & children]
  (apply link dispatch! {:db #(db/open-ask % id) :pull :ask :history :push} (str "/a/" id) attrs children))

(defn- home-view [{:keys [sources loading? ask-history]} dispatch!]
  [:main.single
   [:h1 "Sources"]
   (cond
     (seq sources)
     [:ul.cards
      (for [{:source/keys [id title root]} sources]
        [:li.card {:replicant/key id}
         (node-link dispatch! root {:class "card-title"} title)
         [:p.preview (:node/summary root)]])]
     loading? [:p.muted "Loading…"]
     :else    [:p.muted "No sources yet. Ingest a PDF from the REPL (see dev/user.clj)."])
   (when (seq ask-history)
     [:section
      [:h2 "Recent questions"]
      [:ul.cards.compact
       (for [{:ask/keys [id question title] :as a} (take 8 ask-history)]
         [:li.card {:replicant/key id}
          (ask-link dispatch! a {:class "card-title"} question)
          [:p.preview title]])]])])

(defn- node-article [node dispatch!]
  (let [{:node/keys [title summary text keywords children links ancestors kind]} node
        leaves? (every? #(= :paragraph (:node/kind %)) children)]
    [:article.node
     (when (seq ancestors)
       [:nav.crumbs
        (for [a ancestors]
          [:span {:replicant/key (:node/id a)} (node-link dispatch! a) " › "])])
     (meta-line node)
     [:h1 title]
     (when summary [:p.summary summary])
     (when (and text (= :paragraph kind)) [:p.source-text.full text])
     (when (seq keywords)
       [:div.keywords (for [kw keywords] [:span {:replicant/key kw} (keyword-link dispatch! kw)])])
     (when (seq children)
       [:section
        [:h2 (if leaves? "Source text" "Drill down")]
        [:ol.cards (for [c children] (card dispatch! c))]])
     (when (seq links)
       [:section
        [:h2 "Related"]
        [:ul.cards.compact (for [l links] (card dispatch! l))]])]))

(defn- pdf-canvas-attrs
  "Canvas attrs that (re)draw page `p` of `url` via pdf.js after each render.
  On the JVM (tests, no DOM) the canvas is inert."
  #?(:cljs [url p on-pages] :clj [_url _p _on-pages])
  #?(:cljs {:replicant/on-render (fn [{:replicant/keys [node]}] (pdf/render-page! node url p on-pages))}
     :clj  {}))

(defn- highlight-attrs
  "Sheet attrs that find the highlight for `key` once (pdf.js text content),
  then scroll the pane to it."
  #?(:cljs [key texts found dispatch!] :clj [_key _texts _found _dispatch!])
  #?(:cljs {:replicant/on-render
            (fn [{:replicant/keys [node]}]
              (if found
                (pdf/scroll-to-highlight! node key (:rects found))
                (pdf/ensure-highlight! key texts
                                       (fn [k result]
                                         (dispatch! {:db #(db/highlight-found % k result)})))))}
     :clj  {}))

(defn- pct [x] (str (* 100 x) "%"))

(defn- source-pane
  "The original document (pdf.js), on the selected node's page, with the
  node's source text highlighted."
  [db node dispatch!]
  (let [sid   (get-in node [:node/source :source/id])
        url   (str "/files/sources/" sid)
        p     (db/pdf-page db)
        n     (db/pdf-pages db sid)
        key   (db/highlight-key db url)
        found (db/highlight db key)
        goto  (fn [page] #(dispatch! {:db (fn [d] (db/pdf-goto d sid page))}))]
    [:aside.source
     [:div.pdf-toolbar
      [:button {:disabled (<= p 1) :on {:click (goto (dec p))}} "‹"]
      [:span.page "Page " p (when n (str " / " n))]
      [:button {:disabled (boolean (and n (>= p n))) :on {:click (goto (inc p))}} "›"]
      (when-not (db/following? db)
        [:button.follow {:on {:click #(dispatch! {:db db/pdf-follow})}} "Back to selection"])]
     [:div.pdf-page
      [:div.pdf-sheet (cond-> (highlight-attrs key (db/highlight-texts node) found dispatch!)
                        found (assoc :style {:aspect-ratio (str (:aspect found))}))
       [:canvas (pdf-canvas-attrs url p (fn [pages]
                                          (when (not= pages n)
                                            (dispatch! {:db #(db/pdf-loaded % sid pages)}))))]
       [:div.pdf-marks
        (for [[i {:keys [x y w h]}] (map-indexed vector (:rects found))]
          [:div.mark {:replicant/key i
                      :style {:left (pct x) :top (pct y) :width (pct w) :height (pct h)}}])]]]]))

(defn- node-view [db dispatch!]
  (if-let [node (db/current-node db)]
    [:main.split
     [:section.content (node-article node dispatch!)]
     (source-pane db node dispatch!)]
    [:main.single [:p.muted (if (:loading? db) "Loading…" "Not found.")]]))

(defn- keyword-view [{:keys [keyword keyword-results loading?]} dispatch!]
  (let [nodes (get keyword-results keyword)]
    [:main.single
     [:span.meta "Keyword"]
     [:h1 keyword]
     (cond
       (seq nodes) [:ul.cards (for [n nodes] (card dispatch! n))]
       loading?    [:p.muted "Loading…"]
       :else       [:p.muted "No nodes carry this keyword."])]))

;;=============================================================================
;; Ask
;;=============================================================================

(defn answer-parts
  "Split answer text into [:text s] and [:cite n] parts."
  [s]
  (for [tok (re-seq #"\[\d+\]|[^\[]+|\[" s)]
    (if-let [[_ n] (re-matches #"\[(\d+)\]" tok)]
      [:cite (parse-long n)]
      [:text tok])))

(defn- path-line [{:node/keys [path kind page]}]
  [:span.meta (str/join " › " (map :node/title (rest path)))
   (when (seq (rest path)) " · ")
   (kind-label kind) (when page (str " · p. " page))])

(defn- answer-view [{:ask/keys [answer cited]} dispatch!]
  [:div.answer
   (for [[i para] (map-indexed vector (str/split answer #"\n\s*\n"))]
     [:p {:replicant/key i}
      (for [[j [kind v]] (map-indexed vector (answer-parts para))]
        (if (= :cite kind)
          (if-let [n (get cited (dec v))]
            (node-link dispatch! n {:class "cite" :title (:node/title n) :replicant/key j} (str v))
            [:span.cite.gone {:replicant/key j :title "No longer in the knowledge base"} (str v)])
          [:span {:replicant/key j} v]))])])

(defn- snippet [s n]
  (when s (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- subgraph-tree
  "The cited nodes with the paths that lead to them, as a nested outline."
  [dispatch! nodes]
  [:ul.tree
   (for [{:node/keys [id kind title summary text] :keys [cite children] :as n} nodes]
     [:li {:replicant/key id :class (when cite "cited")}
      [:div.tree-row
       (when cite [:span.cite-num (str cite)])
       (node-link dispatch! n {:class "tree-title"}
                  (if (= :paragraph kind) (snippet text 90) title))
       (meta-line n)]
      (when cite
        (if (= :paragraph kind)
          [:p.source-text (snippet text 600)]
          (when summary [:p.preview summary])))
      (when (seq children) (subgraph-tree dispatch! children))])])

(defn- hit-cards [dispatch! hits]
  [:ul.cards.compact
   (for [{:node/keys [id kind title summary text] :as n} hits]
     [:li.card {:replicant/key id}
      (node-link dispatch! n {:class "card-title"} (if (= :paragraph kind) (snippet text 120) title))
      (path-line n)
      (when (not= :paragraph kind) [:p.preview summary])])])

(defn- ask-view [{:keys [asking search-hits loading?] :as db} dispatch!]
  (let [ask      (db/current-ask db)
        question (or (:ask/question ask) asking)]
    [:main.single.ask
     [:span.meta "Question"]
     [:h1 question]
     (cond
       ask
       (list
        (answer-view ask dispatch!)
        (when (seq (:ask/followups ask))
          [:div.followups {:replicant/key "followups"}
           (for [q (:ask/followups ask)]
             [:button.chip {:replicant/key q :on {:click #(dispatch! (ask-effects q))}} q])])
        (when (seq (:ask/tree ask))
          [:section {:replicant/key "tree"}
           [:h2 "In the knowledge base"]
           (subgraph-tree dispatch! (:ask/tree ask))])
        (when (seq (:ask/matches ask))
          [:section {:replicant/key "matches"}
           [:h2 "Also matched"]
           (hit-cards dispatch! (:ask/matches ask))]))

       asking
       (list
        [:p.muted.thinking {:replicant/key "thinking"} "Answering from the knowledge base…"]
        (when-let [hits (seq (get search-hits asking))]
          [:section {:replicant/key "hits"}
           [:h2 "Matches"]
           (hit-cards dispatch! hits)]))

       loading? [:p.muted "Loading…"]
       :else    [:p.muted "Not found."])]))

(defn app-view [db dispatch!]
  [:div.app
   (header db dispatch!)
   (error-banner db dispatch!)
   (case (:view db)
     :node    (node-view db dispatch!)
     :keyword (keyword-view db dispatch!)
     :ask     (ask-view db dispatch!)
     (home-view db dispatch!))])

^:rct/test
(comment
  (answer-parts "A [1]. B [2][10] [x] end")
  ;=> [[:text "A "] [:cite 1] [:text ". B "] [:cite 2] [:cite 10] [:text " "] [:text "["] [:text "x] end"]]
  )
