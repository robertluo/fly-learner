(ns robertluo.fly-learner.ui.html
  "Draws a source whose pages are HTML (an EPUB or a folder of Markdown,
  cut into pages by the server: robertluo.fly-learner.ingest/page-html). The page is the
  document's own text, so it is its text layer too: selectable, and found
  by its text nodes. Its type scales with the page's width (style.css), so
  the page lays out the same at any zoom, as a PDF page does, and boxes in
  fractions of it stay where they were.

  Formulas (an element with data-tex, its text the TeX in its delimiters:
  robertluo.fly-learner.ingest.tex) are typeset by KaTeX once the pages are
  fetched. A page's text is its formulas' TeX, not KaTeX's drawing of it, so
  it is what the knowledge base read."
  (:require ["katex" :as katex]
            [clojure.string :as str]))

(defn- typeset
  "Page `html` with its formulas typeset."
  [html]
  (if-not (str/includes? html "data-tex")
    html
    (let [t (js/document.createElement "template")]
      (set! (.-innerHTML t) html)
      (doseq [^js el (array-seq (.querySelectorAll (.-content t) "[data-tex]"))]
        (let [display? (= "display" (.getAttribute el "data-tex"))
              n        (if (and display? (str/starts-with? (.-textContent el) "$$")) 2 1)
              s        (str/trim (.-textContent el))
              tex      (subs s n (max n (- (count s) n)))]
          (set! (.-innerHTML el) (.renderToString katex tex #js {:displayMode display? :throwOnError false}))))
      (.-innerHTML t))))

(defonce ^:private docs (atom {})) ; url -> Promise<[html ..]>

(defn document
  "Promise of the pages at `url` (fetched once, then cached): [html ..]."
  [url]
  (or (get @docs url)
      (let [p (-> (js/fetch url)
                  (.then (fn [^js r] (if (.-ok r) (.json r) (throw (js/Error. (str url ": " (.-status r)))))))
                  (.then #(mapv typeset (js->clj %)))
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

(defn- read-text?
  "Is text node `n` part of the page's text? Of a typeset formula, only its
  TeX (the annotation KaTeX keeps in its MathML), not the glyphs drawn."
  [^js n]
  (let [^js p (.-parentElement n)]
    (cond (nil? p)                          true
          (.closest p ".katex-html")        false
          (.closest p ".katex-mathml")      (= "annotation" (str/lower-case (.-tagName p)))
          :else                             true)))

(defn- text-nodes [^js el]
  (let [w (js/document.createTreeWalker el js/NodeFilter.SHOW_TEXT
                                        #js {:acceptNode #(if (read-text? %) js/NodeFilter.FILTER_ACCEPT js/NodeFilter.FILTER_REJECT)})]
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
