(ns robertluo.fly-learner.ui.core.views
  "The page: Replicant hiccup from the state. Components take db and
  dispatch!, and dispatch effect maps ({:db f :pull op :history :push}).

  The top-level composition: the header (the map, the concepts, the
  clusters being learned, the top box), the selection popover, and one view
  per route (`app-view`). Each part lives in robertluo.fly-learner.ui.core.views.*:
    template      generic pieces: no state, no effects (icons, score bars, ...)
    nav           links and effects to the app's places
    panes         split views, pannable drawings, full screen
    node          passages: cards, a node's page, keyword pages
    document      the source document beside them, page by page (PDF, EPUB)
    ideas         the learner's ideas and bookmarks
    selection     the popover on selected text
    ask           questions to the knowledge base, answers with citations
    quiz          quizzes and the study page
    concept       a concept's page, a cluster's, the concept index
    graph         the concept graph
    home          the map of every cluster
    explorations  interactive explorations of a passage
    settings      adding sources, the models, the prompts"
  (:require [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.history :as history]
            [robertluo.fly-learner.ui.core.views.ask :as ask-ui]
            [robertluo.fly-learner.ui.core.views.concept :as concept-ui]
            [robertluo.fly-learner.ui.core.views.explorations :as explore-ui]
            [robertluo.fly-learner.ui.core.views.graph :as graph-ui]
            [robertluo.fly-learner.ui.core.views.home :as home-ui]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.node :as node-ui]
            [robertluo.fly-learner.ui.core.views.notebook :as notebook-ui]
            [robertluo.fly-learner.ui.core.views.panes :as panes]
            [robertluo.fly-learner.ui.core.views.document :as doc-ui]
            [robertluo.fly-learner.ui.core.views.quiz :as quiz-ui]
            [robertluo.fly-learner.ui.core.views.selection :as selection-ui]
            [robertluo.fly-learner.ui.core.views.settings :as settings-ui]
            [robertluo.fly-learner.ui.core.views.template :as t]
            #?(:cljs [clojure.string :as str])))

(defn- exercise-effects
  "Open book exercise `x` (from the top box): its passage, the source at its
  page beside, and its quiz docked above, on the first concept it tests."
  [{:exercise/keys [id concepts] nid :node/id}]
  (let [item (some-> concepts first :concept/id db/concept-item)]
    {:db      #(cond-> (db/box-cleared %)
                 nid  (db/open-node nid)
                 item (db/quiz-start item id))
     :pull    (cond-> [:learning] nid (conj :node :ideas) item (conj :question-create))
     :history :push}))

(defn- goto-list
  "Under the top box: the concepts and book exercises its text names. Picked
  on mousedown, before the box loses focus and closes."
  [db dispatch!]
  (when-let [{:keys [concepts exercises]} (db/box-suggestions db)]
    (let [pick (fn [effects] {:mousedown (fn [e] (.preventDefault e) (dispatch! effects))})]
      [:div.goto
       (for [{:exercise/keys [id label summary page] :as x} exercises]
         [:button.goto-item.exercise {:replicant/key (str id) :type "button" :on (pick (exercise-effects x))}
          [:span.tag.authored "exercise"] [:strong "Exercise " label] [:span.muted summary]
          [:span.where "p. " page " · try it"]])
       (for [{:concept/keys [id label gloss matched]} concepts]
         [:button.goto-item {:replicant/key (str id) :type "button" :on (pick (nav/concept-effects id))}
          [:span.tag "concept"] [:strong label] (when matched [:span.muted "(" matched ")"])
          [:span.muted.gloss gloss]
          [:span.where (t/score-bar (db/item-score db (db/concept-item id)))]])
       [:div.goto-hint.muted "Enter: ask the knowledge base"]])))

(defn- submit-ask
  "Form submit: ask the form's `q` input, then clear it."
  #?(:cljs [dispatch! e] :clj [_dispatch! _e])
  #?(:cljs (do (.preventDefault e)
               (let [input (.. e -target -elements -q)
                     q     (str/trim (.-value input))]
                 (when-not (str/blank? q)
                   (set! (.-value input) "")
                   (.blur input)
                   (dispatch! (nav/ask-effects q)))))
     :clj  nil))

(def ^:private gear
  [:svg {:viewBox "0 0 24 24" :width 20 :height 20 :aria-hidden "true" :fill "none" :stroke "currentColor"
         :stroke-width 1.8 :stroke-linecap "round" :stroke-linejoin "round"}
   [:circle {:cx 12 :cy 12 :r 3.2}]
   [:path {:d "M19.4 15a1.7 1.7 0 0 0 .3 1.8l.1.1a2 2 0 1 1-2.8 2.8l-.1-.1a1.7 1.7 0 0 0-1.8-.3 1.7 1.7 0 0 0-1 1.5V21a2 2 0 1 1-4 0v-.1a1.7 1.7 0 0 0-1.1-1.5 1.7 1.7 0 0 0-1.8.3l-.1.1a2 2 0 1 1-2.8-2.8l.1-.1a1.7 1.7 0 0 0 .3-1.8 1.7 1.7 0 0 0-1.5-1H3a2 2 0 1 1 0-4h.1a1.7 1.7 0 0 0 1.5-1.1 1.7 1.7 0 0 0-.3-1.8l-.1-.1a2 2 0 1 1 2.8-2.8l.1.1a1.7 1.7 0 0 0 1.8.3H9a1.7 1.7 0 0 0 1-1.5V3a2 2 0 1 1 4 0v.1a1.7 1.7 0 0 0 1 1.5 1.7 1.7 0 0 0 1.8-.3l.1-.1a2 2 0 1 1 2.8 2.8l-.1.1a1.7 1.7 0 0 0-.3 1.8V9a1.7 1.7 0 0 0 1.5 1H21a2 2 0 1 1 0 4h-.1a1.7 1.7 0 0 0-1.5 1z"}]])

(defn- learning-menu
  "The topics being learned, behind one button: a menu, so there may be any
  number of them. The button names the one open, if any; picking one, a
  click outside or Escape closes it."
  [db dispatch!]
  (let [cs     (db/learning-clusters db)
        open?  (= :learning (:menu db))
        here   (some #(when (= (:cluster/id %) (:cluster-id db)) %) cs)
        close  #(dispatch! {:db db/menu-closed})
        closed (fn [{f :db :as effects}] (assoc effects :db (comp db/menu-closed f)))]
    [:div.menu {:class (when open? "open")
                :on    {:keydown #(when (= "Escape" (.-key %)) (close))}}
     [:button.menu-button {:type          "button"
                           :class         ["learning" (when here "here")]
                           :aria-haspopup "true" :aria-expanded (str open?)
                           :title         "The topics you are learning"
                           :on            {:click #(dispatch! {:db (fn [d] (db/menu-toggled d :learning))})}}
      (if here
        [:span.name (:cluster/label here)]
        "Learning")
      (when (seq cs) [:span.count (if here (t/percent (:score here)) (count cs))])
      [:svg.caret {:viewBox "0 0 10 6" :width 10 :height 6 :aria-hidden "true" :fill "none" :stroke "currentColor"
                   :stroke-width 1.6 :stroke-linecap "round" :stroke-linejoin "round"}
       [:path {:d "M1 1l4 4 4-4"}]]]
     (when open?
       [:div.menu-backdrop {:on {:click close}}])
     (when open?
       [:div.menu-list {:role "menu"}
        (if (seq cs)
          (for [{:cluster/keys [id label] :keys [score] :as c} cs]
            (nav/link dispatch! (closed (nav/cluster-effects id))
                      (history/state->path {:view :cluster :cluster-id id})
                      {:replicant/key (str id) :role "menuitem" :title (or (:cluster/gloss c) label)
                       :class ["menu-item" (when (= id (:cluster-id db)) "here")]}
                      [:span.name label] (t/score-bar score) [:span.pct (t/percent score)]))
          [:p.muted.menu-empty "Nothing yet: pick a topic on the map, then “Learn this topic”."])
        (nav/link dispatch! (closed nav/map-effects) "/"
                  {:class "menu-foot" :role "menuitem"} "Choose topics on the map")])]))

(defn- header
  "The map, the clusters being learned (each with its score), and the
  question box: one way in, the rest follows from the page."
  [{:keys [asking] :as db} dispatch!]
  [:header.top
   (nav/link dispatch! nav/map-effects "/" {:class "brand" :title "The map of everything you know"}
              [:img.brand-icon {:src "/favicon.svg" :alt ""}] "Fly Learner")
   [:nav.top-nav
    (nav/link dispatch! nav/map-effects "/" {:class (when (= :home (:view db)) "here")} "Map")
    (nav/link dispatch! {:db db/open-concepts :pull [:concept-index :ideas] :history :push} "/c"
              {:class (when (= :concepts (:view db)) "here") :title "Every concept, A to Z"} "Concepts")
    (nav/link dispatch! {:db db/open-notebook :pull :ideas :history :push} "/b"
              {:class (when (= :notebook (:view db)) "here") :title "What you wrote while reading, as a book"} "Notebook")
    (learning-menu db dispatch!)]
   [:form.ask {:on {:submit #(submit-ask dispatch! %)}}
    [:input {:name "q" :type "search" :autocomplete "off" :disabled (boolean asking)
             :value (get-in db [:box :q])
             :placeholder (if asking "Answering…" "Find a concept or exercise, or ask…")
             :on {:input   #(dispatch! {:db (fn [d] (db/box-typed d (.. % -target -value))) :pull :goto})
                  :focus   #(dispatch! {:db (fn [d] (db/box-open d true)) :pull :goto})
                  :blur    #(dispatch! {:db (fn [d] (db/box-open d false))})
                  :keydown #(when (= "Escape" (.-key %)) (dispatch! {:db (fn [d] (db/box-open d false))}))}}]
    (goto-list db dispatch!)]
   (nav/link dispatch! {:db db/open-settings :pull :settings :history :push} "/s"
             {:class ["settings-link" (when (= :settings (:view db)) "here") (when (db/ingesting? db) "busy")]
              :title (if (db/ingesting? db) "Settings: a source is being ingested" "Settings: add sources, models, prompts")
              :aria-label "Settings"}
             gear)])

(defn- error-banner [{:keys [error]} dispatch!]
  (when error
    [:div.error
     [:span (or (:reason error) (name (:code error)))]
     [:button {:on {:click #(dispatch! {:db db/clear-error})}} "Dismiss"]]))

(defn- docked
  "The left pane of a split view: a quiz under way docked at its top, then `content`."
  [db dispatch! content]
  [(quiz-ui/quiz-dock db dispatch!) content])

(defn- node-view [db dispatch!]
  (if-let [node (db/current-node db)]
    (panes/split db dispatch!
                 (docked db dispatch! (node-ui/node-article db node dispatch!))
                 (doc-ui/source-pane db node dispatch!))
    [:main.single [:p.muted (if (:loading? db) "Loading…" "Not found.")]]))

(defn- cluster-view [db dispatch!]
  (panes/split db dispatch!
               (docked db dispatch!
                       (if-let [c (db/current-cluster db)]
                         (concept-ui/cluster-article db c dispatch!)
                         [:p.muted (if (or (:loading? db) (nil? (:universe db))) "Loading…" "Not found.")]))
               (graph-ui/concept-graph db dispatch!)))

(defn- concept-view [db dispatch!]
  (panes/split db dispatch!
               (docked db dispatch!
                       (if-let [c (db/current-concept db)]
                         (concept-ui/concept-article db c (db/item-score db (db/concept-item (:concept/id c))) dispatch!)
                         [:p.muted (if (:loading? db) "Loading…" "Not found.")]))
               (graph-ui/concept-graph db dispatch!)))

(defn app-view [db dispatch!]
  [:div.app {:class (when (:fullscreen? db) "fullscreen")}
   (header db dispatch!)
   (error-banner db dispatch!)
   (selection-ui/selection-popover db dispatch!)
   (case (:view db)
     :node    (node-view db dispatch!)
     :keyword (node-ui/keyword-view db dispatch!)
     :term    (concept-ui/term-view db dispatch!)
     :ask      (ask-ui/ask-view db dispatch!)
     :concept  (concept-view db dispatch!)
     :cluster  (cluster-view db dispatch!)
     :concepts (concept-ui/concepts-view db dispatch!)
     :notebook (notebook-ui/notebook-view db dispatch!)
     :study    (quiz-ui/study-view db dispatch!)
     :explore  (explore-ui/explore-view db dispatch!)
     :settings (settings-ui/settings-view db dispatch!)
     (home-ui/home-view db dispatch!))])
