(ns robertluo.fly-learner.ui.core.views.panes
  "Panes: split views (the source or the graph beside the reading), pannable
  and zoomable drawings (the map, the concept graph; see db/pane-view) and
  full screen."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.template :as t]
            #?(:cljs [robertluo.fly-learner.ui.zoom :as zoom])))

(defn fullscreen-button
  "Full screen: the browser, showing only this pane; or back."
  #?(:cljs [db dispatch!] :clj [db _dispatch!])
  (if (:fullscreen? db)
    [:exit-fullscreen "Leave full screen (Esc)"
     #?(:cljs #(zoom/fullscreen! false (fn [] (dispatch! {:db (fn [d] (db/fullscreen d false))})))
        :clj  (fn [_]))]
    [:fullscreen "Full screen"
     #?(:cljs #(zoom/fullscreen! true (fn [] (dispatch! {:db (fn [d] (db/fullscreen d true))})))
        :clj  (fn [_]))]))

(defn pane-attrs
  "A pannable, zoomable SVG (pane `k`, see db/pane-view): it reports its
  size, and dragging, scrolling and pinching pan and zoom it; names too
  small to read are left out. Gestures are inert on the JVM."
  #?(:cljs [db k dispatch!] :clj [db k _dispatch!])
  (let [{:keys [view-box labels?]} (db/pane-view db k)]
    (merge {:viewBox (str/join " " view-box)
            :preserveAspectRatio "xMidYMid meet"
            :class (when-not labels? "no-labels")}
           #?(:cljs (t/on-render
                     (fn [el]
                         (zoom/watch-size! el (fn [w h] (dispatch! {:db #(db/pane-sized % k w h)})))
                         (zoom/pan-zoom! el {:on-pan  (fn [dx dy] (dispatch! {:db #(db/pane-panned % k dx dy)}))
                                             :on-zoom (fn [f at] (dispatch! {:db #(db/pane-zoomed % k f at)}))})))
              :clj  {}))))

(defn pane-zoom-controls
  "Zoom in, out, the whole drawing, and full screen, for pane `k`."
  [db k dispatch! whole-title]
  (t/zoom-controls [[:plus "Zoom in" #(dispatch! {:db (fn [d] (db/pane-zoomed d k 1.4))})]
                    [:minus "Zoom out" #(dispatch! {:db (fn [d] (db/pane-zoomed d k (/ 1 1.4)))})]
                    [:whole whole-title #(dispatch! {:db (fn [d] (db/pane-whole d k))})]
                    (fullscreen-button db dispatch!)]))

(defn width-toggle
  "Give the source (right pane) the whole width, or share it again."
  [{:keys [source-only?]} dispatch! what]
  [:button.width-toggle {:title (if source-only? (str "Show the " what " again") (str "Hide the " what ": the source gets the whole width"))
                         :on {:click #(dispatch! {:db db/source-only-toggle})}}
   (if source-only? (str "⇥ Show " what) "⇤ Full width")])

(defn split
  "Two panes side by side (stacked on a small screen): `left` (a vector of
  children) and `right`; the left one hidden when the source should have
  the whole width, else as wide as the learner dragged the divider."
  [db dispatch! left right]
  (let [f (db/split-share db)]
    [:main.split {:class (when (:source-only? db) "source-only")
                  :style {:grid-template-columns (str "minmax(0, " f "fr) minmax(0, " (- 1 f) "fr)")}}
     (into [:section.content] left)
     right
     (when-not (db/wide? db)
       (t/split-handle f {:on-drag    (fn [share] (dispatch! {:db #(db/split-dragged % share)}))
                          :on-release (fn [_] (dispatch! {:db #(db/split-set % nil)}))
                          :on-reset   (fn [] (dispatch! {:db #(db/split-set % 0.5)}))}))]))
