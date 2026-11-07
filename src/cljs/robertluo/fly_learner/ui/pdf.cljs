(ns robertluo.fly-learner.ui.pdf
  "pdf.js interop. The library is self-hosted under /vendor/pdfjs (bb vendor)
  and loaded by a module script in index.html, which sets window.pdfjsLib and
  fires `pdfjs:ready`."
  (:require [robertluo.fly-learner.ui.core.highlight :as hl]))

(defn- lib
  "Promise of the pdf.js module."
  []
  (js/Promise.
   (fn [resolve _]
     (if-let [l (.-pdfjsLib js/window)]
       (resolve l)
       (.addEventListener js/window "pdfjs:ready"
                          #(resolve (.-pdfjsLib js/window)) #js {:once true})))))

(defonce ^:private docs (atom {})) ; url -> Promise<PDFDocumentProxy>

(defn document
  "Promise of the loaded document at `url` (loaded once, then cached)."
  [url]
  (or (get @docs url)
      (let [p (-> (lib)
                  (.then (fn [^js l] (.-promise (.getDocument l #js {:url url}))))
                  (.catch (fn [e] (swap! docs dissoc url) (throw e))))]
        (swap! docs assoc url p)
        p)))

(def ^:private max-canvas-pixels 8e6)

(defn render-page!
  "Draw page `p` of `url` into `canvas`, fitted to the width of its parent.
  Cheap to call on every render: skips when that page is already drawn at
  that width. Calls (on-doc num-pages aspect) once the page is loaded,
  aspect being its width / height."
  [^js canvas url p on-doc]
  (let [width (some-> (.-parentElement canvas) .-clientWidth)
        key   (str url "#" p "@" width)]
    ;; no parent: the canvas was just removed from the page
    (when (and width (pos? width) (not= key (.. canvas -dataset -rendered)))
      (set! (.. canvas -dataset -rendered) key)
      (some-> ^js (.-renderTask canvas) .cancel)
      (-> (document url)
          (.then (fn [^js doc] (.then (.getPage doc p) (fn [page] [doc page]))))
          (.then (fn [[^js doc ^js page]]
                   (let [vp1 (.getViewport page #js {:scale 1})]
                     (on-doc (.-numPages doc) (/ (.-width vp1) (.-height vp1))))
                   ;; a newer render may have started meanwhile
                   (when (= key (.. canvas -dataset -rendered))
                     (let [vp1   (.getViewport page #js {:scale 1})
                           scale (/ width (.-width vp1))
                           ;; zoomed in, a page at full resolution would take hundreds of MB
                           dpr   (min (or (.-devicePixelRatio js/window) 1)
                                      (Math/sqrt (/ max-canvas-pixels
                                                    (* width (/ (* width (.-height vp1)) (.-width vp1))))))
                           vp    (.getViewport page #js {:scale (* scale dpr)})
                           task  (do (set! (.-width canvas) (.-width vp))
                                     (set! (.-height canvas) (.-height vp))
                                     ;; no CSS size of its own: the sheet's width sizes it
                                     ;; (else, until redrawn, a zoomed-out page keeps its old size)
                                     (.render page #js {:canvas canvas :viewport vp}))]
                       (set! (.-renderTask canvas) task)
                       (.-promise task)))))
          (.catch (fn [^js e]
                    (when-not (= "RenderingCancelledException" (.-name e))
                      ;; allow the next render to retry
                      (when (= key (.. canvas -dataset -rendered))
                        (js-delete (.-dataset canvas) "rendered"))
                      (js/console.error "pdf.js:" e))))))))

;;=============================================================================
;; Text layer: the page's text over the canvas, selectable
;;=============================================================================

(defn render-text-layer!
  "Fill `container` (a .textLayer over the page) with page `p`'s text, laid
  out by pdf.js to match the canvas, so it can be selected. Cheap to call on
  every render: skips when that page is already laid out at that width.
  Calls (on-ready) once the text is in place."
  [^js container url p on-ready]
  (let [width (some-> (.-parentElement container) .-clientWidth)
        key   (str url "#" p "@" width)]
    (when (and width (pos? width) (not= key (.. container -dataset -rendered)))
      (set! (.. container -dataset -rendered) key)
      (-> (js/Promise.all #js [(lib) (document url)])
          (.then (fn [[^js l ^js doc]] (.then (.getPage doc p) (fn [page] [l page]))))
          (.then (fn [[^js l ^js page]]
                   (when (= key (.. container -dataset -rendered))
                     (let [scale (/ width (.-width (.getViewport page #js {:scale 1})))]
                       (set! (.-innerHTML container) "")
                       (.setProperty (.-style container) "--total-scale-factor" (str scale))
                       (.setProperty (.-style container) "--scale-round-x" "1px")
                       (.setProperty (.-style container) "--scale-round-y" "1px")
                       (-> (.render ^js (new (.-TextLayer l)
                                         #js {:textContentSource (.streamTextContent page)
                                              :container container
                                              :viewport (.getViewport page #js {:scale scale})}))
                           (.then (fn [] (when (= key (.. container -dataset -rendered)) (on-ready)))))))))
          (.catch (fn [e]
                    (js-delete (.-dataset container) "rendered")
                    (js/console.error "pdf.js text layer:" e)))))))

;;=============================================================================
;; Highlights
;;=============================================================================

(defn- span-rect
  "Box of chars [from, to) of text item `item`, in page fractions (top-left
  origin). Horizontal text only; nil otherwise."
  [^js vp ^js item from to]
  (let [[a b c _ e f] (.-transform item)
        len (count (.-str item))
        w   (.-width item)
        h   (.-height item)]
    (when (and (zero? b) (zero? c) (pos? a) (pos? len) (pos? h))
      (let [x0 (+ e (* w (/ from len)))
            x1 (+ e (* w (/ to len)))
            ;; baseline f; cover descenders and most of the ascent
            ;; pdf.js 6 has no convertToViewportRectangle: convert two corners
            [vx0 vy0] (.convertToViewportPoint vp x0 (- f (* 0.25 h)))
            [vx1 vy1] (.convertToViewportPoint vp x1 (+ f (* 0.85 h)))
            [x y] [(min vx0 vx1) (min vy0 vy1)]]
        {:x (/ x (.-width vp))
         :y (/ y (.-height vp))
         :w (/ (abs (- vx1 vx0)) (.-width vp))
         :h (/ (abs (- vy1 vy0)) (.-height vp))}))))

(defn- find-highlight
  "Promise of {:aspect w/h :rects [...]}: where `texts` are on page `p`."
  [url p texts]
  (-> (document url)
      (.then (fn [^js doc] (.getPage doc p)))
      (.then (fn [^js page]
               (-> (.getTextContent page)
                   (.then (fn [^js tc]
                            (let [vp    (.getViewport page #js {:scale 1})
                                  items (.-items tc)
                                  runs  (hl/find-spans (mapv #(.-str ^js %) items) texts)]
                              {:aspect (/ (.-width vp) (.-height vp))
                               :rects  (vec (mapcat (fn [run]
                                                      (hl/merge-rects
                                                       (keep (fn [{:keys [item from to]}]
                                                               (span-rect vp (aget items item) from to))
                                                             run)))
                                                    runs))}))))))))

(defonce ^:private pending (atom nil))

(defn ensure-highlight!
  "Compute the highlight for `key` ([url page node-id]) unless it is done or
  under way; calls (on-found key result)."
  [key texts on-found]
  (when (not= key @pending)
    (reset! pending key)
    (let [[url p] key]
      (-> (find-highlight url p texts)
          (.then (fn [result] (on-found key result)))
          (.catch (fn [e]
                    (when (= key @pending) (reset! pending nil))
                    (js/console.error "pdf.js highlight:" e)))))))

;;=============================================================================
;; Scrolling: every page in one column (.pdf-page, of .pdf-sheet[data-page])
;;=============================================================================

(def ^:private probe
  "How far below the pane's top edge the line is whose page is the current one."
  56)

(defn- sheet [^js pane p] (.querySelector pane (str ".pdf-sheet[data-page=\"" p "\"]")))

(defn- page-at
  "The page under the probe line: the first sheet ending below it."
  [^js pane]
  (let [y (+ (.-scrollTop pane) probe)]
    (some (fn [^js s]
            (when (> (+ (.-offsetTop s) (.-offsetHeight s)) y)
              (parse-long (.. s -dataset -page))))
          (js/Array.from (.querySelectorAll pane ".pdf-sheet")))))

(defn- remember-spot!
  "Where in its page the pane is (a fraction of the page at the probe line)
  and across (the middle, as a fraction of the width), to return there when
  the pages change size."
  [^js pane]
  (when-let [^js s (sheet pane (.. pane -dataset -at))]
    (set! (.. pane -dataset -fy) (/ (- (+ (.-scrollTop pane) probe) (.-offsetTop s)) (max 1 (.-offsetHeight s))))
    (set! (.. pane -dataset -fx) (/ (+ (.-scrollLeft pane) (/ (.-clientWidth pane) 2)) (max 1 (.-scrollWidth pane))))))

(defn- scroll-pane!
  "Scroll `pane` to `top`; the scroll events this causes are not the learner's."
  [^js pane top smooth?]
  (set! (.-ignoreUntil pane) (+ (js/performance.now) (if smooth? 700 120)))
  (.scrollTo pane #js {:top (max 0 top) :behavior (if smooth? "smooth" "instant")})
  (remember-spot! pane))

(defn sync-scroll!
  "Keep `pane` at page `page`. Scrolls there when the page was changed other
  than by scrolling (pane.dataset.at is the page the scroll position is at);
  when the pages changed size (`layout`, a string: the document, its page
  count, the pane's width, the zoom), back to the same spot of the page.
  `y`, a fraction of the page, is where the highlight `key` (a string) is
  when it should be in view: scrolled to once, smoothly when already on its
  page."
  [^js pane {:keys [page y key layout]}]
  (let [ds       (.-dataset pane)
        turned?  (not= (str page) (.-at ds))
        resized? (not= layout (.-layout ds))
        new-y?   (and y (not= key (.-placed ds)))]
    (when-let [^js s (and (or turned? resized? new-y?) (sheet pane page))]
      (set! (.-layout ds) layout)
      (cond
        (or turned? (and new-y? (not resized?)))
        (do (set! (.-at ds) (str page))
            (when y (set! (.-placed ds) key))
            (scroll-pane! pane
                          (if y
                            (- (+ (.-offsetTop s) (* y (.-clientHeight s))) 48)
                            (- (.-offsetTop s) 16))
                          (not turned?)))

        :else
        (let [fy (js/parseFloat (.-fy ds)), fx (js/parseFloat (.-fx ds))]
          (when y (set! (.-placed ds) key))
          (when-not (js/isNaN fx)
            (set! (.-scrollLeft pane) (- (* fx (.-scrollWidth pane)) (/ (.-clientWidth pane) 2))))
          (scroll-pane! pane
                        (if (js/isNaN fy)
                          (- (.-offsetTop s) 16)
                          (- (+ (.-offsetTop s) (* fy (.-offsetHeight s))) probe))
                        false))))))

(defn watch-scroll!
  "Once per pane: as the learner scrolls, (on-page p) when another page
  reaches the probe line, and (on-settle following?) once those changes
  pause; following? says whether the pane followed the node (its
  data-following) when the first of them happened."
  [^js pane on-page on-settle]
  (when-not (.-watching pane)
    (set! (.-watching pane) true)
    (let [frame (volatile! nil), settle (volatile! nil), following (volatile! nil)]
      (.addEventListener
       pane "scroll"
       (fn []
         (when-not @frame
           (vreset! frame
                    (js/requestAnimationFrame
                     (fn []
                       (vreset! frame nil)
                       (when (> (js/performance.now) (or (.-ignoreUntil pane) 0))
                         (when-let [p (page-at pane)]
                           (when (not= (str p) (.. pane -dataset -at))
                             (set! (.. pane -dataset -at) (str p))
                             (when (nil? @following)
                               (vreset! following (= "true" (.. pane -dataset -following))))
                             (on-page p)
                             (js/clearTimeout @settle)
                             (vreset! settle (js/setTimeout (fn []
                                                              (on-settle @following)
                                                              (vreset! following nil))
                                                            400))))))
                       ;; after the page it is on may have changed
                       (remember-spot! pane)))))
       #js {:passive true}))))
