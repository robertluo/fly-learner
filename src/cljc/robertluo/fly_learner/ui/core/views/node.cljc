(ns robertluo.fly-learner.ui.core.views.node
  "Passages: a node as a card in a list, a node's page (summary, key points,
  drill-down, ideas, explorations offered) and a keyword's page."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.api :as api]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(def ^:private kind-label
  {:source "Overview" :section "Section" :group "Passage" :paragraph "Source text"})

(defn meta-line [{:node/keys [kind page]}]
  [:span.meta (kind-label kind) (when page (str " · p. " page))])

(defn- card
  "A node in a list: title link, kind/page, summary or source text. With
  `in` ({:db .. :source-id ..}), source text can be selected to react to,
  and shows the quotes of ideas on it."
  ([dispatch! n] (card dispatch! n nil))
  ([dispatch! {:node/keys [id summary text kind] :as n} {:keys [db] :as in}]
   (let [expandable? (and db summary (not= :paragraph kind))
         open?       (and expandable? (db/card-open? db id))
         toggle      #(dispatch! {:db (fn [d] (db/card-toggled d id))})]
     [:li.card {:replicant/key id :class (when expandable? "expandable")}
      (when expandable?
        [:button.card-toggle {:title (if open? "Fold it" "Read it all") :aria-expanded (str open?)
                              :on {:click toggle}}
         (if open? "▴" "▾")])
      (nav/node-link dispatch! n {:class "card-title"}
                     (if (= :paragraph kind) (meta-line n) (:node/title n)))
      (when-not (= :paragraph kind) (meta-line n))
      (cond
        (and (= :paragraph kind) in) (ideas-ui/anchored-text :p.source-text in id :text text)
        (= :paragraph kind)          [:p.source-text text]
        expandable?                  [:p.preview {:class (when open? "full")
                                                  :title (when-not open? "Click to read it all")
                                                  :on {:click toggle}}
                                      summary]
        summary                      [:p.preview summary])])))

(defn- key-points
  "The key sentences of a passage: label and sentence; a click shows it in
  the source (the PDF scrolled to it)."
  [db {:node/keys [keypoints children source]} dispatch!]
  (let [sid    (:source/id source)
        url    (api/source-url sid)
        page-of (into {} (map (juxt :node/id :node/page)) children)]
    [:ol.key-list
     (for [[i {:keys [para quote label] :as kp}] (map-indexed vector keypoints)
           :let [page  (page-of para)
                 found (when page (db/keypoint-at db url sid page kp))]]
       [:li {:replicant/key i}
        [:button.key-point
         {:title (str "Show it in the source" (when page (str ", p. " page)))
          :on {:click #(when page
                         (let [[p y] (or found [page nil])]
                           (dispatch! {:db (fn [d] (db/pdf-goto-target d sid p y))
                                       :history (if (db/following? db) :push :replace)})))}}
         [:span.key-label-l label]
         [:span.key-quote "“" quote "”"]
         ;; the page it was found on, once placed (its paragraph may start the page before)
         (when page [:span.meta "p. " (or (first found) page)])]])]))

(defn making-note [what]
  [:p.making [:span.spinner] what " Writing and checking it takes about half a minute."])

(defn- node-explore
  "What a node offers to explore: the explorations made of it, and making
  one (when the knowledge can make it: a proposal)."
  [{:keys [exploring]} {:node/keys [id explore]} dispatch!]
  (let [{:keys [proposal explorations]} explore
        making? (= id (:node exploring))]
    (when (or proposal (seq explorations))
      [:section.explore-offer
       [:h2 "Try it out"]
       (when (seq explorations)
         [:ul.plain
          (for [{:exploration/keys [id title]} explorations]
            [:li {:replicant/key (str id)} (nav/explore-link dispatch! id {} "▶ " title)])])
       (cond
         making? (making-note "Building an exploration of this passage.")
         proposal [:div.offer
                   [:p (:pitch proposal)]
                   [:button.btn.primary {:disabled (some? exploring)
                                         :on {:click #(dispatch! {:db (fn [d] (db/explore-node d id))
                                                                  :pull :exploration-create})}}
                    (if (seq explorations) "Make another" "Explore it")]])])))

(defn node-article [db node dispatch!]
  (let [{:node/keys [id title summary text keywords concepts children links ancestors kind source]} node
        leaves? (every? #(= :paragraph (:node/kind %)) children)
        in      {:db db :source-id (:source/id source)}]
    [:article.node
     (when (seq ancestors)
       [:nav.crumbs
        (for [a ancestors]
          [:span {:replicant/key (:node/id a)} (nav/node-link dispatch! a) " › "])])
     (meta-line node)
     [:h1 title]
     (when summary (ideas-ui/anchored-text :p.summary in id :summary summary))
     (when (and text (= :paragraph kind)) (ideas-ui/anchored-text :p.source-text.full in id :text text))
     (when (or (seq concepts) (seq keywords))
       [:div.keywords
        (for [c concepts] (nav/concept-chip dispatch! c))
        (for [kw keywords] [:span {:replicant/key kw} (nav/keyword-link dispatch! kw)])])
     (when (= :source kind)
       (when-let [bms (seq (db/bookmarks db (:source/id source)))]
         [:section
          [:h2 "Continue reading"]
          (ideas-ui/bookmark-list dispatch! bms false)]))
     (node-explore db node dispatch!)
     (cond
       ;; a passage: its key sentences, the paragraphs folded away
       (and leaves? (seq (:node/keypoints node)))
       [:section.key-points
        [:h2 "Key points"]
        (key-points db node dispatch!)
        [:details.paragraphs
         [:summary (count children) " paragraphs"]
         [:ol.cards (for [c children] (card dispatch! c in))]]]

       (seq children)
       [:section
        (if leaves?
          [:h2 "Source text"]
          (let [all? (get-in db [:cards :all?])]
            [:div.section-head
             [:h2 "Drill down"]
             [:button.btn.quiet {:on {:click #(dispatch! {:db (fn [d] (db/cards-all d (not all?)))})}}
              (if all? "Collapse all" "Expand all")]]))
        [:ol.cards (for [c children] (card dispatch! c in))]])
     (ideas-ui/ideas-panel db dispatch! (db/node-anchor node)
                           (db/ideas-in-node db id [:page (:source/id source) (db/pdf-page db)]))
     (when (seq links)
       [:section
        [:h2 "Related"]
        [:ul.cards.compact (for [l links] (card dispatch! l))]])]))

(defn keyword-view [{:keys [keyword keyword-results loading?]} dispatch!]
  (let [nodes (get keyword-results keyword)]
    [:main.single
     [:span.meta "Keyword"]
     [:h1 keyword]
     (cond
       (seq nodes) [:ul.cards (for [n nodes] (card dispatch! n))]
       loading?    [:p.muted "Loading…"]
       :else       [:p.muted "No nodes carry this keyword."])]))

(defn- path-line [{:node/keys [path kind page]}]
  [:span.meta (str/join " › " (map :node/title (rest path)))
   (when (seq (rest path)) " · ")
   (kind-label kind) (when page (str " · p. " page))])

(defn hit-cards [dispatch! hits]
  [:ul.cards.compact
   (for [{:node/keys [id kind title summary text] :as n} hits]
     [:li.card {:replicant/key id}
      (nav/node-link dispatch! n {:class "card-title"} (if (= :paragraph kind) (t/snippet text 120) title))
      (path-line n)
      (when (not= :paragraph kind) [:p.preview summary])])])
