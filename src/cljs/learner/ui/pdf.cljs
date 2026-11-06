(ns learner.ui.pdf
  "pdf.js interop. The library is self-hosted under /vendor/pdfjs (bb vendor)
  and loaded by a module script in index.html, which sets window.pdfjsLib and
  fires `pdfjs:ready`."
  (:require [learner.ui.core.highlight :as hl]))

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

(defn render-page!
  "Draw page `p` of `url` into `canvas`, fitted to the width of its parent.
  Cheap to call on every render: skips when that page is already drawn at
  that width. Calls (on-doc num-pages) once the document is loaded."
  [^js canvas url p on-doc]
  (let [width (.-clientWidth (.-parentElement canvas))
        key   (str url "#" p "@" width)]
    (when (and (pos? width) (not= key (.. canvas -dataset -rendered)))
      (set! (.. canvas -dataset -rendered) key)
      (some-> ^js (.-renderTask canvas) .cancel)
      (-> (document url)
          (.then (fn [^js doc]
                   (on-doc (.-numPages doc))
                   (.getPage doc p)))
          (.then (fn [^js page]
                   ;; a newer render may have started meanwhile
                   (when (= key (.. canvas -dataset -rendered))
                     (let [scale (/ width (.-width (.getViewport page #js {:scale 1})))
                           dpr   (or (.-devicePixelRatio js/window) 1)
                           vp    (.getViewport page #js {:scale (* scale dpr)})
                           task  (do (set! (.-width canvas) (.-width vp))
                                     (set! (.-height canvas) (.-height vp))
                                     (set! (.. canvas -style -width) (str width "px"))
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

(defn scroll-to-highlight!
  "Scroll the pane (the sheet's scrolling parent) to the first highlighted
  box, or to the top when there is none. Once per `key`."
  [^js sheet key rects]
  (let [k (pr-str key)]
    (when (not= k (.. sheet -dataset -scrolled))
      (set! (.. sheet -dataset -scrolled) k)
      (let [pane ^js (.-parentElement sheet)
            top  (if-let [y (some :y rects)]
                   (let [offset (- (.-top (.getBoundingClientRect sheet))
                                   (.-top (.getBoundingClientRect pane))
                                   (- (.-scrollTop pane)))]
                     (max 0 (- (+ offset (* y (.-clientHeight sheet))) 48)))
                   0)]
        (.scrollTo pane #js {:top top :behavior "smooth"})))))
