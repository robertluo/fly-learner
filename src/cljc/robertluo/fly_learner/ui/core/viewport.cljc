(ns robertluo.fly-learner.ui.core.viewport
  "A pannable, zoomable view of a drawing (an SVG): the part of it, a box
  [x y w h] in drawing units, that a pane of [w h] pixels shows.

  State: {:size [w h] :manual {:scale s :center [x y]}}; :scale is pixels
  per drawing unit. Until the learner pans or zooms (:manual) the view is
  automatic: the whole drawing if its text is readable so, else readable
  text around a focus point. Text smaller than readable is not worth
  drawing (labels? says whether to).

  opts: :font, text size in drawing units; :focus [x y] or nil; :max-auto,
  the largest automatic scale (a small drawing is not blown up).")

(def readable-px
  "The smallest text worth drawing, in pixels."
  10.0)

(def ^:private max-scale 4.0)

(defn- fit-scale [[_ _ w h] [pw ph]] (min (/ (double pw) w) (/ (double ph) h)))

(defn- middle [[x y w h]] [(+ x (/ w 2)) (+ y (/ h 2))])

(defn- automatic [box size {:keys [font focus max-auto] :or {max-auto 1.2}}]
  (let [fit   (fit-scale box size)
        scale (-> fit (min max-auto) (max (/ readable-px font)))]
    {:scale  scale
     :center (if (or (>= fit scale) (nil? focus)) (middle box) focus)}))

(defn current
  "{:scale :center} shown now, or nil before the pane's size is known."
  [{:keys [size manual]} box opts]
  (when size
    (or manual (when box (automatic box size opts)))))

(defn view-box
  "The SVG viewBox [x y w h] for `view` in a pane of `size`."
  [{:keys [scale center]} [pw ph]]
  (let [[cx cy] center, w (/ pw scale), h (/ ph scale)]
    [(- cx (/ w 2)) (- cy (/ h 2)) w h]))

(defn labels?
  "Whether text of `font` drawing units is readable at `view`."
  [view font]
  (or (nil? view) (>= (* (:scale view) font) readable-px)))

(defn sized [state w h] (assoc state :size [w h]))

(defn panned
  "Dragged by `dx` `dy` pixels."
  [state box opts dx dy]
  (if-let [{:keys [scale] [x y] :center :as v} (current state box opts)]
    (assoc state :manual (assoc v :center [(- x (/ dx scale)) (- y (/ dy scale))]))
    state))

(defn zoomed
  "Zoomed by `factor`, the point at pixel `at` ([x y] in the pane, its
  middle when nil) staying where it is; no further out than the whole
  drawing or readable text, whichever is smaller."
  [{:keys [size] :as state} box opts factor at]
  (if-let [{:keys [scale] [cx cy] :center} (current state box opts)]
    (let [[pw ph]  size
          [px py]  (or at [(/ pw 2) (/ ph 2)])
          ox (- px (/ pw 2)), oy (- py (/ ph 2))
          lo (min (fit-scale box size) (/ readable-px (:font opts)))
          s' (-> (* scale factor) (max lo) (min max-scale))]
      ;; the drawing point under the pointer, before and after
      (assoc state :manual {:scale  s'
                            :center [(- (+ cx (/ ox scale)) (/ ox s'))
                                     (- (+ cy (/ oy scale)) (/ oy s'))]}))
    state))

(defn whole
  "The whole drawing, readable or not."
  [{:keys [size] :as state} box]
  (if (and size box)
    (assoc state :manual {:scale (min max-scale (fit-scale box size)) :center (middle box)})
    state))

(defn automatic-again [state] (dissoc state :manual))

^:rct/test
(comment
  (def opts {:font 20})
  ;; a drawing that fits with readable text is shown whole (not blown up)
  (current {:size [800 600]} [0 0 400 300] opts) ;=> {:scale 1.2 :center [200 150]}
  ;; a large one: readable text, around the focus
  (def big (current {:size [800 600]} [0 0 8000 6000] (assoc opts :focus [100 100])))
  (* (:scale big) 20) ;=> 10.0
  (:center big) ;=> [100 100]
  (current {} [0 0 1 1] opts) ;=> nil
  (labels? {:scale 0.1} 20) ;=> false
  (view-box {:scale 2 :center [0 0]} [800 600]) ;=> [-200 -150 400 300]

  ;; zooming keeps the point under the pointer where it is, and is manual from then on
  (def z (zoomed {:size [800 600]} [-400 -300 800 600] opts 2 [600 300]))
  (:manual z) ;=> {:scale 2.0 :center [100.0 0.0]}
  (:center (:manual (panned {:size [800 600] :manual {:scale 2.0 :center [0 0]}} nil opts 100 -50))) ;=> [-50.0 25.0]
  ;; never further out than the whole drawing or readable text
  (:scale (:manual (zoomed {:size [800 600]} [0 0 8000 6000] opts 0.01 nil))) ;=> 0.1
  (:manual (whole {:size [800 600]} [0 0 1600 600])) ;=> {:scale 0.5 :center [800 300]}
  (automatic-again {:size [1 1] :manual {}}) ;=> {:size [1 1]}
  )
