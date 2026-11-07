(ns robertluo.fly-learner.ui.zoom
  "Pan and zoom gestures, DOM-side; what they do to the state is up to the
  caller. Each attaches once per element.

    watch-size!  (on-size w h) when the element's size changes (and at first)
    pan-zoom!    dragging and trackpad scrolling pan, pinching (ctrl/⌘ +
                 wheel) zooms at the pointer; a drag swallows its click
    pinch!       only pinching (ctrl/⌘ + wheel): for panes that scroll
    fullscreen!  the browser full screen, or back")

(defn watch-size! [^js el on-size]
  (when-not (.-sizeWatched el)
    (set! (.-sizeWatched el) true)
    (.observe (js/ResizeObserver.
               (fn [_]
                 (let [w (.-clientWidth el), h (.-clientHeight el)]
                   (when (and (pos? w) (pos? h)) (on-size w h)))))
              el)))

(defn- zoom-factor
  "Zoom for a wheel event: pinching gives small deltas, a wheel big ones."
  [^js e]
  (Math/exp (* -0.01 (max -50 (min 50 (.-deltaY e))))))

(defn pan-zoom!
  "On `el`: (on-pan dx dy) in pixels, (on-zoom factor [x y]) with the
  pointer relative to `el`; at most once per frame each."
  [^js el {:keys [on-pan on-zoom]}]
  (when-not (.-panZoom el)
    (set! (.-panZoom el) true)
    (let [acc   (volatile! {:dx 0 :dy 0 :f 1})
          frame (volatile! nil)
          drag  (volatile! nil)
          flush (fn []
                  (vreset! frame nil)
                  (let [{:keys [dx dy f at]} @acc]
                    (vreset! acc {:dx 0 :dy 0 :f 1})
                    (when (or (not (zero? dx)) (not (zero? dy))) (on-pan dx dy))
                    (when (not= 1 f) (on-zoom f at))))
          soon  #(when-not @frame (vreset! frame (js/requestAnimationFrame flush)))]
      (.addEventListener
       el "wheel"
       (fn [^js e]
         (.preventDefault e)
         (if (or (.-ctrlKey e) (.-metaKey e))
           (let [r (.getBoundingClientRect el)]
             (vswap! acc #(-> % (update :f * (zoom-factor e))
                              (assoc :at [(- (.-clientX e) (.-left r)) (- (.-clientY e) (.-top r))]))))
           (vswap! acc #(-> % (update :dx - (.-deltaX e)) (update :dy - (.-deltaY e)))))
         (soon))
       #js {:passive false})
      (.addEventListener el "pointerdown"
                         (fn [^js e]
                           (when (zero? (.-button e))
                             (vreset! drag {:x (.-clientX e) :y (.-clientY e) :id (.-pointerId e)}))))
      (.addEventListener el "pointermove"
                         (fn [^js e]
                           (when-let [{:keys [x y id moved?]} @drag]
                             (let [dx (- (.-clientX e) x), dy (- (.-clientY e) y)]
                               ;; a few pixels of wobble is still a click
                               (when (or moved? (> (+ (abs dx) (abs dy)) 4))
                                 (when-not moved? (.setPointerCapture el id))
                                 (vreset! drag {:x (.-clientX e) :y (.-clientY e) :id id :moved? true})
                                 (vswap! acc #(-> % (update :dx + dx) (update :dy + dy)))
                                 (soon))))))
      (doseq [ev ["pointerup" "pointercancel"]]
        (.addEventListener el ev (fn [_]
                                   (when (:moved? @drag) (set! (.-dragEnded el) (js/performance.now)))
                                   (vreset! drag nil))))
      ;; the click that ends a drag is not a click on what was under it
      (.addEventListener el "click"
                         (fn [^js e]
                           (when (< (- (js/performance.now) (or (.-dragEnded el) -1e9)) 300)
                             (.stopPropagation e)
                             (.preventDefault e)))
                         true))))

(defn pinch!
  "On `el` (which scrolls by itself): (on-zoom factor) for pinching or
  ctrl/⌘ + wheel, gathered over a short while (zooming may redraw a lot)."
  [^js el on-zoom]
  (when-not (.-pinch el)
    (set! (.-pinch el) true)
    (let [f (volatile! 1), t (volatile! nil)]
      (.addEventListener
       el "wheel"
       (fn [^js e]
         (when (or (.-ctrlKey e) (.-metaKey e))
           (.preventDefault e)
           (vswap! f * (zoom-factor e))
           (when-not @t
             (vreset! t (js/setTimeout (fn [] (vreset! t nil) (on-zoom @f) (vreset! f 1)) 120)))))
       #js {:passive false}))))

(defn fullscreen!
  "Put the browser full screen (`on?`) or back. The whole page goes full
  screen (what shows of it is up to the app: popovers keep working); the
  app learns of the change from `fullscreenchange`. Where full screen is
  not available (a phone), (fallback) is called instead."
  [on? fallback]
  (let [doc js/document]
    (cond
      (and on? (.-fullscreenEnabled doc))
      (-> (.requestFullscreen (.-documentElement doc)) (.catch (fn [_] (fallback))))

      (and (not on?) (.-fullscreenElement doc))
      (.exitFullscreen doc)

      :else (fallback))))
