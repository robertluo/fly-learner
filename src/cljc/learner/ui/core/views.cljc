(ns learner.ui.core.views
  "UI views: Replicant hiccup. Components receive db and dispatch!, and
  dispatch effect maps ({:db f :pull op :history :push})."
  (:require [learner.ui.core.db :as db]
            [learner.ui.core.history :as history]
            #?(:cljs [learner.ui.pdf :as pdf])))

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

(defn- header [dispatch!]
  [:header.top
   (link dispatch! {:db db/go-home :history :push} "/" {:class "brand"} "Learner")])

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

(defn- home-view [{:keys [sources loading?]} dispatch!]
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
     :else    [:p.muted "No sources yet. Ingest a PDF from the REPL (see dev/user.clj)."])])

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

(defn- source-pane
  "The original document (pdf.js), on the selected node's page."
  [db node dispatch!]
  (let [sid   (get-in node [:node/source :source/id])
        url   (str "/files/sources/" sid)
        p     (db/pdf-page db)
        n     (db/pdf-pages db sid)
        goto  (fn [page] #(dispatch! {:db (fn [d] (db/pdf-goto d sid page))}))]
    [:aside.source
     [:div.pdf-toolbar
      [:button {:disabled (<= p 1) :on {:click (goto (dec p))}} "‹"]
      [:span.page "Page " p (when n (str " / " n))]
      [:button {:disabled (boolean (and n (>= p n))) :on {:click (goto (inc p))}} "›"]
      (when-not (db/following? db)
        [:button.follow {:on {:click #(dispatch! {:db db/pdf-follow})}} "Back to selection"])]
     [:div.pdf-page
      [:canvas (pdf-canvas-attrs url p (fn [pages]
                                         (when (not= pages n)
                                           (dispatch! {:db #(db/pdf-loaded % sid pages)}))))]]]))

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

(defn app-view [db dispatch!]
  [:div.app
   (header dispatch!)
   (error-banner db dispatch!)
   (case (:view db)
     :node    (node-view db dispatch!)
     :keyword (keyword-view db dispatch!)
     (home-view db dispatch!))])
