(ns robertluo.fly-learner.ui.core.views.template
  "Generic Replicant templates: hiccup from plain values and callbacks.
  They know nothing of the app's state, effects or routes, so any view can
  use them (and they can be restyled or replaced alone).")

#?(:cljs
   (defn on-render
     "A :replicant/on-render hook calling (f node) when the element is mounted or
     updated, never when it is removed: then it is detached, and a hook that
     throws leaves Replicant's view of the page out of step with the page."
     [f]
     {:replicant/on-render (fn [{:replicant/keys [node life-cycle]}]
                             (when-not (= :replicant.life-cycle/unmount life-cycle)
                               (f node)))}))

(defn- icon
  "A 16px line icon: `d` an SVG path."
  [d]
  [:svg.icon {:viewBox "0 0 16 16" :width 16 :height 16 :aria-hidden "true"}
   [:path {:d d :fill "none" :stroke "currentColor" :stroke-width 1.6
           :stroke-linecap "round" :stroke-linejoin "round"}]])

(def ^:private icons
  {:plus  "M8 3v10M3 8h10"
   :minus "M3 8h10"
   :whole "M2 5V2h3M11 2h3v3M14 11v3h-3M5 14H2v-3M6 6h4v4H6z"
   :fullscreen "M2 6V2h4M2 2l4 4M10 2h4v4M14 2l-4 4M14 10v4h-4M14 14l-4-4M6 14H2v-4M2 14l4-4"
   :exit-fullscreen "M2 6h4V2M6 6 2 2M10 2v4h4M10 6l4-4M14 10h-4v4M10 10l4 4M6 14v-4H2M6 10l-4 4"
   :width "M2 8h12M5 5 2 8l3 3M11 5l3 3-3 3"})

(defn zoom-controls
  "Icon buttons floating at the bottom right of a pane: [[icon title
  on-click disabled?] ..] (nil: none); the title is the tooltip (and the
  accessible name)."
  [buttons]
  [:div.zoom-controls
   (for [[k title on-click disabled?] (remove nil? buttons)]
     [:button {:replicant/key (name k) :title title :aria-label title
               :disabled (boolean disabled?) :on {:click on-click}}
      (icon (icons k))])])

(defn percent
  "A score as a percentage: whole numbers, one decimal below 10%."
  [x]
  (let [p (* 100 (or x 0))]
    (if (and (pos? p) (< p 10))
      (str (/ (Math/round (* 10.0 p)) 10.0) "%")
      (str (Math/round (double p)) "%"))))

(defn score-bar [score]
  [:span.score-bar [:span.fill {:style {:width (str (* 100 (or score 0)) "%")}}]])

(defn popover-style
  "Centred under the selection, kept inside the viewport; a wide popover
  (an answer) opens above a selection low in the viewport."
  [{:keys [x y top vh]} wide?]
  (let [w (if wide? 480 320)
        above? (and wide? vh top (> y (* 0.55 vh)))]
    (cond-> {:left (str "clamp(8px, " (- x (/ w 2)) "px, calc(100vw - " (+ w 8) "px))")}
      above?       (assoc :bottom (str (+ (- vh top) 8) "px") :max-height (str (- top 16) "px"))
      (not above?) (assoc :top (str (+ y 8) "px") :max-height (str "calc(100vh - " (+ y 16) "px)")))))

(defn translate [x y] (str "translate(" x "," y ")"))

(defn pct [x] (str (* 100 x) "%"))

(defn split-handle
  "The divider between two panes, at share `f` of the width: dragging it
  calls (on-drag share) at most once a frame, letting go (on-release), a
  double click (on-reset)."
  #?(:cljs [f {:keys [on-drag on-release on-reset]}] :clj [f _callbacks])
  [:div.split-handle
   {:title "Drag to resize the panes; double-click for halves"
    :role "separator" :aria-orientation "vertical" :aria-valuenow (Math/round (* 100 f))
    :style {:left (str "calc(" (* 100 f) "% - 4px)")}
    :on #?(:clj {}
           :cljs {:dblclick (fn [_] (on-reset))
                  :pointerdown
                  (fn [^js e]
                    (when (zero? (.-button e))
                      (.preventDefault e)
                      (let [handle (.-currentTarget e)
                            main   (.-parentElement handle)
                            frame  (volatile! nil)
                            move   (fn [^js m]
                                     (let [r (.getBoundingClientRect main) x (.-clientX m)]
                                       (when-not @frame
                                         (vreset! frame (js/requestAnimationFrame
                                                         (fn [] (vreset! frame nil)
                                                           (on-drag (/ (- x (.-left r)) (.-width r)))))))))
                            up     (fn up [_]
                                     (.removeEventListener handle "pointermove" move)
                                     (.removeEventListener handle "pointerup" up)
                                     (.remove (.-classList js/document.body) "resizing")
                                     (js/requestAnimationFrame on-release))]
                        (.setPointerCapture handle (.-pointerId e))
                        (.add (.-classList js/document.body) "resizing")
                        (.addEventListener handle "pointermove" move)
                        (.addEventListener handle "pointerup" up))))})}])

(defn snippet
  "`s` cut to `n` characters, an ellipsis marking the cut."
  [s n]
  (when s (if (> (count s) n) (str (subs s 0 n) "…") s)))
