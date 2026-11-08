(ns robertluo.fly-learner.ui.core.views.home
  "The map of everything: clusters (topics) placed by how they connect,
  sized by concepts, filled by how well they are known."
  (:require [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.panes :as panes]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(defn- map-bubble [dispatch! {:keys [id label lines gloss n score learning? x y r]}]
  ;; the same children every time (see graph-node)
  [:g.bubble {:replicant/key (str id)
              :class (when learning? "learning")
              :transform (t/translate x y)
              :on {:click (fn [_] (dispatch! (nav/cluster-effects id)))}}
   [:title (str label (when gloss (str ": " gloss)) " (" n " concepts; you know " (t/percent score) ")")]
   [:circle.ring {:r (+ r 6)}]
   [:circle.body {:r r}]
   [:circle.heat {:r r :style {:fill-opacity (if (pos? score) (+ 0.15 (* 0.85 score)) 0)}}]
   [:text.n {:dy "0.35em" :text-anchor "middle" :font-size 18} (str n)]
   [:text.label {:y (+ r (* 1.2 db/map-font)) :text-anchor "middle" :font-size db/map-font}
    (for [[i line] (map-indexed vector lines)]
      [:tspan {:replicant/key i :x 0 :dy (if (zero? i) 0 "1.15em")} line])]])

(defn- knowledge-map [db dispatch!]
  (let [{:keys [bubbles links]} (db/map-scene db)]
    (if (seq bubbles)
      [:svg.kmap (panes/pane-attrs db :map dispatch!)
       [:g.links
        (for [[i {:keys [x1 y1 x2 y2 weight]}] (map-indexed vector links)]
          [:line {:replicant/key i :x1 x1 :y1 y1 :x2 x2 :y2 y2
                  :style {:stroke-width (min 6 (+ 1 (* 0.5 weight)))}}])]
       [:g.bubbles (for [b bubbles] (map-bubble dispatch! b))]]
      [:p.muted.graph-empty
       (if (:universe db)
         "No topics yet: ingest a source (bb ingest data/sources/<file>.pdf)."
         "Loading the map…")])))

(defn home-view
  "The map of everything: topics (clusters of concepts) placed by how they
  connect, whatever their sources, sized by concepts, filled by how well they are known. Pick topics to learn."
  [{:keys [sources ask-history learning] :as db} dispatch!]
  (let [learning-now (db/learning-clusters db)]
    [:main.map-home
     [:aside.map-side
      [:span.meta "Everything you know"]
      (when-let [o (:overall learning)]
        [:div.known [:span.big (t/percent o)] (t/score-bar o)])
      [:section
       [:h2 "Learning now"]
       (if (seq learning-now)
         [:ul.learning-list
          (for [{:cluster/keys [id] :keys [score] :as c} learning-now]
            [:li {:replicant/key (str id)}
             (nav/cluster-link dispatch! c {:class "card-title"})
             (t/score-bar score) [:span.pct (t/percent score)]])]
         [:p.hint.muted "Pick a topic on the map, then “Learn this topic”. Learn a few at a time."])]
      (when-let [bms (seq (db/bookmarks db))]
        [:section
         [:h2 "Bookmarks"]
         (ideas-ui/bookmark-list dispatch! bms true)])
      (when (seq sources)
        [:section
         [:h2 "Sources"]
         [:ul.plain
          (for [{:source/keys [id title root]} sources]
            [:li {:replicant/key (str id)} (nav/node-link dispatch! root {} title)])]])
      (when (seq ask-history)
        [:section
         [:h2 "Recent questions"]
         [:ul.plain
          (for [{:ask/keys [id question] :as a} (take 6 ask-history)]
            [:li {:replicant/key (str id)} (nav/ask-link dispatch! a {} question)])]])]
     [:section.map-pane
      [:div.graph-toolbar
       [:span (count (get-in db [:universe :clusters])) " topics"]
       [:span.legend [:span.key.heat] "known" [:span.key.learning] "learning"]
       [:span.hint "Click a topic to open it."]]
      (knowledge-map db dispatch!)
      (when (seq (get-in db [:universe :clusters]))
        (panes/pane-zoom-controls db :map dispatch! "The whole map"))]]))
