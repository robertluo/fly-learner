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
  "{:anchor quote-anchor :x :y} for the current selection (x, y: where to
  offer a reaction, in viewport pixels), or nil when nothing anchorable is
  selected."
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
               :x (+ (.-left rect) (/ (.-width rect) 2))
               :y (.-bottom rect)})))))))

(defn clear! [] (some-> (js/window.getSelection) .removeAllRanges))
