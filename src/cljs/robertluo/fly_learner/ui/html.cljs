(ns robertluo.fly-learner.ui.html
  "Draws a source whose pages are HTML (an EPUB, cut into pages by the
  server: robertluo.fly-learner.ingest.epub/page-html). The page is the
  document's own text, so it is its text layer too: selectable, and found
  by its text nodes. Its type scales with the page's width (style.css), so
  the page lays out the same at any zoom, as a PDF page does, and boxes in
  fractions of it stay where they were.")

(defonce ^:private docs (atom {})) ; url -> Promise<[html ..]>

(defn document
  "Promise of the pages at `url` (fetched once, then cached): [html ..]."
  [url]
  (or (get @docs url)
      (let [p (-> (js/fetch url)
                  (.then (fn [^js r] (if (.-ok r) (.json r) (throw (js/Error. (str url ": " (.-status r)))))))
                  (.then js->clj)
                  (.catch (fn [e] (swap! docs dissoc url) (throw e))))]
        (swap! docs assoc url p)
        p)))

(defonce ^:private loading (atom #{}))

(defn load!
  "Fetch the pages at `url` unless under way; calls (on-loaded [html ..])."
  [url on-loaded]
  (when-not (@loading url)
    (swap! loading conj url)
    (-> (document url)
        (.then on-loaded)
        (.catch (fn [e] (js/console.error "source pages:" e)))
        (.finally #(swap! loading disj url)))))

(defn- page-element
  "The element holding page `p`'s HTML in the column `el` is in."
  [^js el p]
  (some-> (.closest el ".doc-pages")
          (.querySelector (str ".doc-sheet[data-page=\"" p "\"] .html-page"))))

(defn- settled
  "Promise of `page` once laid out for good: its images loaded, the fonts in."
  [^js page]
  (js/Promise.all
   (into-array
    (cons (.-ready js/document.fonts)
          (for [^js img (array-seq (.querySelectorAll page "img")) :when (not (.-complete img))]
            (js/Promise. (fn [ok _] (.addEventListener img "load" ok) (.addEventListener img "error" ok))))))))

(defn when-laid-out!
  "(f) once page element `page` (filled) is laid out for good."
  [^js page f]
  (.then (settled page) (fn [_] (when (.-isConnected page) (f)))))

(defn- text-nodes [^js el]
  (let [w (js/document.createTreeWalker el js/NodeFilter.SHOW_TEXT)]
    (loop [out []] (if-let [n (.nextNode w)] (recur (conj out n)) out))))

(defn page-text
  "Promise of page `p`'s text ({:strs :rects :aspect}, as
  robertluo.fly-learner.ui.pdf/page-text), for the column `el` is in: its
  text nodes, boxes in fractions of the page. Fails when the page is not
  there (yet)."
  [^js el p]
  (if-let [page (when el (page-element el p))]
    (.then (settled page)
           (fn [_]
             (let [box   (.getBoundingClientRect page)
                   nodes (text-nodes page)]
               (when-not (and (pos? (.-width box)) (pos? (.-height box)))
                 (throw (js/Error. (str "page " p " not laid out"))))
               {:strs   (mapv #(.-textContent ^js %) nodes)
                :rects  (fn [i from to]
                          (let [r (doto (js/document.createRange)
                                    (.setStart (nth nodes i) from)
                                    (.setEnd (nth nodes i) to))]
                            (->> (array-seq (.getClientRects r))
                                 (filter #(and (> (.-width ^js %) 0.5) (> (.-height ^js %) 0.5)))
                                 (map (fn [^js b] {:x (/ (- (.-left b) (.-left box)) (.-width box))
                                                   :y (/ (- (.-top b) (.-top box)) (.-height box))
                                                   :w (/ (.-width b) (.-width box))
                                                   :h (/ (.-height b) (.-height box))})))))
                :aspect (/ (.-width box) (.-height box))})))
    (js/Promise.reject (js/Error. (str "page " p " not shown")))))
