(ns learner.ui.pdf
  "pdf.js interop. The library is self-hosted under /vendor/pdfjs (bb vendor)
  and loaded by a module script in index.html, which sets window.pdfjsLib and
  fires `pdfjs:ready`.")

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
