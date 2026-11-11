(ns robertluo.fly-learner.ui.core.views.graph
  "The concept graph: the concepts shown, their edges (prerequisite edges
  with an arrow halfway), how well each is known, the learner's ideas."
  (:require [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.panes :as panes]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(defn- graph-node
  [dispatch! scale {:keys [id label gloss mentions x y r focus? selected? expanded? hidden score]}]
  [:g {:replicant/key id
       :class (cond-> ["cnode"]
                focus?    (conj "focus")
                selected? (conj "selected")
                expanded? (conj "expanded"))
       :transform (t/translate x y)
       :on {:click (fn [_] (dispatch! {:db #(db/graph-click % id) :pull :concept
                                       :history :push :animate true}))}}
   [:title (str label (when gloss (str ": " gloss)) " (" mentions " passages; you know " (t/percent score) ")")]
   ;; the same children every time, shown or hidden by class: a keyed node that
   ;; moves (the selected one is drawn last) and gains a child in the same render
   ;; leaves Replicant's view of the DOM out of step, and old edges stay drawn
   [:circle.halo {:r (+ r 5)}]
   [:circle.dot {:r r}]
   ;; the heat: how well it is known
   [:circle.heat {:r r :style {:fill-opacity (if (pos? score) (+ 0.15 (* 0.85 score)) 0)}}]
   [:text.label {:y (+ r (* 14 scale)) :text-anchor "middle" :font-size (* 12 scale)} label]
   (when (or expanded? (pos? hidden))
     [:g.badge {:transform (str (t/translate (* 0.75 r) (* -0.75 r)) " scale(" scale ")")
                :on {:click (fn [e]
                              (.stopPropagation e)
                              (dispatch! {:db #(db/graph-toggle % id) :animate true}))}}
      [:title (if expanded? "Collapse" (str "Expand: " hidden " more"))]
      [:circle {:r 8}]
      [:text {:dy "0.35em" :text-anchor "middle"} (if expanded? "−" "+")]])])

(defn- graph-edge [{:keys [from to kind weight shared why x1 y1 x2 y2 score]}]
  (if (= :requires kind)
    ;; from the prerequisite (to) to what needs it (from), the arrow halfway
    [:polyline {:replicant/key (str from "-" to "-r")
                :class ["edge" "requires"]
                :points (str x2 "," y2 " " (/ (+ x1 x2) 2) "," (/ (+ y1 y2) 2) " " x1 "," y1)
                :marker-mid "url(#prereq-arrow)"}
     [:title (str "learn first" (when why (str ": " why)))]]
    [:line {:replicant/key (str from "-" to)
            :class (cond-> ["edge" (name kind)] (pos? score) (conj "known"))
            :x1 x1 :y1 y1 :x2 x2 :y2 y2
            :style (if (pos? score)
                     {:stroke-width (+ 2 (* 4 score)) :stroke-opacity (+ 0.4 (* 0.6 score))}
                     (when (= :related kind) {:stroke-opacity (+ 0.25 (min 0.6 (* 1.5 (or weight 0))))}))}
     [:title (str (if (= :broader kind) "is a kind or part of" (str shared " shared passages"))
                  "; you know " (t/percent score))]]))

(defn- prereq-arrow
  "The arrowhead of a prerequisite edge, sized to stay readable as labels do."
  [scale]
  [:defs
   [:marker {:id "prereq-arrow" :viewBox "0 0 10 10" :refX 5 :refY 5 :orient "auto"
             :markerUnits "userSpaceOnUse" :markerWidth (* 9 scale) :markerHeight (* 9 scale)}
    [:path {:d "M0,0 L10,5 L0,10 z"}]]])

(defn concept-graph [db dispatch!]
  (let [{:keys [nodes edges scale] {idea-dots :ideas idea-links :links} :ideas} (db/graph-scene db)
        id (:concept-id db)]
    [:aside.source.graph-pane
     [:div.graph-toolbar
      (panes/width-toggle db dispatch! (if (= :cluster (:view db)) "topic" "concept"))
      [:span (count nodes) (if (= 1 (count nodes)) " concept" " concepts")]
      [:span.legend
       [:span.key.broader] "broader above" [:span.key.requires] "learn first, above" [:span.key.related] "related"
       [:span.key.heat] "known"]
      (when-let [o (:overall (:learning db))]
        [:span.overall {:title "Of the whole graph, weighted by importance"} "You know " (t/percent o)])
      [:label.toggle [:input {:type "checkbox" :checked (boolean (get-in db [:ideas :layer?]))
                              :on {:change #(dispatch! {:db db/ideas-layer-toggle})}}]
       "My ideas"]
      (when (and id (= :concept (:view db)))
        [:button {:on {:click #(dispatch! {:db (fn [d] (db/graph-reset d id)) :animate true})}
                  :title (if (get-in db [:graph :scope])
                           "Leave the topic: show this concept and its neighbours"
                           "Show only this concept and its neighbours")}
         "Reset"])]
     (if (seq nodes)
       [:svg.cgraph (panes/pane-attrs db :graph dispatch!)
        (prereq-arrow scale)
        [:g.edges (map graph-edge edges)]
        [:g.nodes (for [n nodes] (graph-node dispatch! scale n))]
        [:g.ideas
         (for [[i [[x1 y1] [x2 y2]]] (map-indexed vector idea-links)]
           [:line.idea-link {:replicant/key i :x1 x1 :y1 y1 :x2 x2 :y2 y2}])
         (for [{{:idea/keys [id stance text]} :idea x :x y :y} idea-dots]
           [:g.idea-dot {:replicant/key (str id) :class (name stance)
                         :transform (str (t/translate x y) " scale(" scale ")")}
            [:title (str (ideas-ui/stance-label stance) (when text (str ": " text)))]
            [:rect {:x -4.5 :y -4.5 :width 9 :height 9 :transform "rotate(45)"}]])]]
       [:p.muted.graph-empty.loading "Loading the concept graph…"])
     (when (seq nodes) (panes/pane-zoom-controls db :graph dispatch! "The whole graph"))
     [:p.graph-hint (if (get-in db [:graph :scope])
                      "The topic's concepts. Click one to open it. Filled: how well you know it."
                      (list "Click a concept to open and expand it, click it again to collapse. "
                            "+ / − expand or collapse without opening. Filled: how well you know it."))]]))
