(ns robertluo.fly-learner.ui.select
  "The browser's text selection as a quote anchor (robertluo.fly-learner.ideas.knowledge).

  Selectable text sits in elements marked data-anchor, which say whose text
  it is: data-source, data-field (text | summary | pdf), and data-node or
  data-page."
  (:require [robertluo.fly-learner.ideas :as ideas]))

(def min-length 3)

(defn- anchor-element [^js node]
  (some-> (if (= 3 (.-nodeType node)) (.-parentElement node) node)
          (.closest "[data-anchor]")))

(defn- pieces
  "`el`'s text as [node text] pieces in document order: text nodes as they
  are, a line break (<br>, as pdf.js puts between lines) as a space."
  [^js el]
  (let [w (js/document.createTreeWalker el (bit-or js/NodeFilter.SHOW_TEXT js/NodeFilter.SHOW_ELEMENT))]
    (loop [out []]
      (if-let [n (.nextNode w)]
        (recur (cond
                 (= 3 (.-nodeType n)) (conj out [n (.-textContent n)])
                 (= "BR" (.-tagName n)) (conj out [n " "])
                 :else out))
        out))))

(defn- flat-text [ps] (apply str (map second ps)))

(defn- offset
  "Index in the flat text (see `pieces`) of the boundary (`container`, `off`):
  every piece starting before it counts in full, but a text node holding
  the boundary counts up to `off`. (Within an element's children, no text
  node straddles the boundary.)"
  [ps ^js container off]
  (let [r (doto (js/document.createRange) (.setStart container off) (.collapse true))]
    (reduce (fn [n [^js node text]]
              (cond
                (identical? node container)        (+ n off)
                (neg? (.comparePoint r node 0))    (+ n (count text))
                :else                              n))
            0 ps)))

(defn selection-anchor
  "{:anchor quote-anchor :x :y :top :vh} for the current selection (x, y:
  where to offer a reaction, in viewport pixels; top: the selection's top;
  vh: the viewport's height), or nil when nothing anchorable is selected."
  []
  (let [sel (js/window.getSelection)]
    (when (and sel (not (.-isCollapsed sel)) (pos? (.-rangeCount sel)))
      (let [range (.getRangeAt sel 0)
            el    (anchor-element (.-commonAncestorContainer range))]
        (when el
          (let [ps    (pieces el)
                text  (flat-text ps)
                start (offset ps (.-startContainer range) (.-startOffset range))
                end   (offset ps (.-endContainer range) (.-endOffset range))
                sel   (ideas/quote-selector text start end)
                ds    (.-dataset el)
                rect  (.getBoundingClientRect range)]
            (when (>= (count (:quote sel)) min-length)
              {:anchor (cond-> (merge {:anchor/kind :quote
                                       :source/id (uuid (.-source ds))
                                       :field (keyword (.-field ds))}
                                      sel)
                         (seq (.-node ds)) (assoc :node/id (uuid (.-node ds)))
                         (seq (.-page ds)) (assoc :page (js/parseInt (.-page ds) 10)))
               :x   (+ (.-left rect) (/ (.-width rect) 2))
               :y   (.-bottom rect)
               :top (.-top rect)
               :vh  js/window.innerHeight})))))))

(defn clear! [] (some-> (js/window.getSelection) .removeAllRanges))

(defn- boundary!
  "Set `range`'s start (or end) at index `i` of the flat text of pieces `ps`."
  [^js range ps i end?]
  (loop [[[^js node text] & more] ps, at 0]
    (when node
      (let [len (count text), stop (+ at len)]
        (if (if end? (and (< at i) (<= i stop)) (and (<= at i) (< i stop)))
          (if (= 3 (.-nodeType node))
            (if end? (.setEnd range node (- i at)) (.setStart range node (- i at)))
            ;; a <br>: just before or after it
            (if end? (.setEndAfter range node) (.setStartBefore range node)))
          (recur more stop))))))

(defn quote-rects
  "Where the quote `selector` is in element `el` (as `selection-anchor` reads
  its text): boxes [{:x :y :w :h}] as fractions of `el`'s box, or nil."
  [^js el selector]
  (let [ps (pieces el)]
    (when-let [[start end] (ideas/find-quote (flat-text ps) selector)]
      (let [range (js/document.createRange)
            box   (.getBoundingClientRect el)]
        (boundary! range ps start false)
        (boundary! range ps end true)
        (when (and (pos? (.-width box)) (pos? (.-height box)))
          (->> (array-seq (.getClientRects range))
               (filter #(and (> (.-width %) 0.5) (> (.-height %) 0.5)))
               (mapv (fn [^js r] {:x (/ (- (.-left r) (.-left box)) (.-width box))
                                  :y (/ (- (.-top r) (.-top box)) (.-height box))
                                  :w (/ (.-width r) (.-width box))
                                  :h (/ (.-height r) (.-height box))}))
               not-empty))))))
