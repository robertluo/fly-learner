(ns robertluo.fly-learner.ui.pdf
  "pdf.js interop. The library is self-hosted under /vendor/pdfjs (bb vendor)
  and loaded by a module script in index.html, which sets window.pdfjsLib and
  fires `pdfjs:ready`.

  Draws a PDF source's pages (robertluo.fly-learner.ui.core.views.document):
  a page on a canvas, its selectable text layer over it, and its text items
  for robertluo.fly-learner.ui.pages to find texts among.")

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
;; Text: where a page's text is, for robertluo.fly-learner.ui.pages
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

(defn page-text
  "Promise of page `p` of `url`'s text: {:strs [..] :rects (fn [i from to]
  [rect ..]) :aspect w/h}, :strs its text items (pdf.js text content), rects
  of chars [from, to) of item i in page fractions."
  [url p]
  (-> (document url)
      (.then (fn [^js doc] (.getPage doc p)))
      (.then (fn [^js page]
               (.then (.getTextContent page)
                      (fn [^js tc]
                        (let [vp    (.getViewport page #js {:scale 1})
                              items (.-items tc)]
                          {:strs   (mapv #(.-str ^js %) items)
                           :rects  (fn [i from to] (some-> (span-rect vp (aget items i) from to) vector))
                           :aspect (/ (.-width vp) (.-height vp))})))))))
