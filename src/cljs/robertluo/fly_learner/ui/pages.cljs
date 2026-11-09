(ns robertluo.fly-learner.ui.pages
  "The source document as one scrolling column of pages, whatever draws
  them: pdf.js a PDF's (robertluo.fly-learner.ui.pdf), the browser an
  EPUB's (robertluo.fly-learner.ui.html). A source is {:kind :pdf|:html
  :url ..}; each kind gives a page's text as items and where chars of an
  item are, in fractions of the page: from there, finding a node's text,
  key sentences, and scrolling are the same for both."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.highlight :as hl]
            [robertluo.fly-learner.ui.html :as html]
            [robertluo.fly-learner.ui.pdf :as pdf]))

;;=============================================================================
;; Finding texts on a page
;;=============================================================================

(defn- page-text
  "Promise of page `p`'s text: {:strs [..] :rects (fn [i from to] [rect ..])
  :aspect w/h}. `el`: an element of the column (where an HTML page is)."
  [{:keys [kind url]} p el]
  (case kind
    :html (html/page-text el p)
    (pdf/page-text url p)))

(defn- run-rects [rects run]
  (hl/merge-rects (mapcat (fn [{:keys [item from to]}] (rects item from to)) run)))

(defn- find-highlight
  "Promise of {:aspect w/h :rects [...]}: where `texts` are on page `p`."
  [doc p el texts]
  (.then (page-text doc p el)
         (fn [{:keys [strs rects aspect]}]
           {:aspect aspect
            :rects  (vec (mapcat #(run-rects rects %) (hl/find-spans strs texts)))})))

(defonce ^:private finding (atom #{}))

(defn find-each!
  "Where each of `texts` is on page `p` of `doc`: calls (on-found {:rects
  [rects-or-nil ..] :left x :aspect a}), rects one per text, in order (key
  sentences, each with its own label), x the left edge of the page's text
  (its margin ends there). Once at a time for the same page and texts."
  [doc p el texts on-found]
  (let [k [(:url doc) p texts]]
    (when-not (@finding k)
      (swap! finding conj k)
      (-> (page-text doc p el)
          (.then (fn [{:keys [strs rects aspect]}]
                   (on-found
                    {:aspect aspect
                     :left   (->> (map-indexed vector strs)
                                  (keep (fn [[i s]] (when-not (str/blank? s) (:x (first (rects i 0 (count s)))))))
                                  ;; the margin: where lines start, past a stray item hanging left
                                  hl/text-left)
                     :rects  (mapv (fn [t] (some->> (hl/best-run strs t) (run-rects rects) not-empty)) texts)})))
          (.catch (fn [e] (js/console.error "source key sentences:" e)))
          (.finally #(swap! finding disj k))))))

(defonce ^:private pending (atom nil))

(defn ensure-highlight!
  "Compute the highlight for `key` ([url page node-id]) of `doc` unless it is
  done or under way; calls (on-found key result)."
  [doc key el texts on-found]
  (when (not= key @pending)
    (reset! pending key)
    (let [[_ p] key]
      (-> (find-highlight doc p el texts)
          (.then (fn [result] (on-found key result)))
          (.catch (fn [e]
                    (when (= key @pending) (reset! pending nil))
                    (js/console.error "source highlight:" e)))))))

;;=============================================================================
;; Scrolling: every page in one column (.doc-pages, of .doc-sheet[data-page])
;;=============================================================================

(def ^:private probe
  "How far below the pane's top edge the line is whose page is the current one."
  56)

(defn- sheet [^js pane p] (.querySelector pane (str ".doc-sheet[data-page=\"" p "\"]")))

(defn- page-at
  "The page under the probe line: the first sheet ending below it."
  [^js pane]
  (let [y (+ (.-scrollTop pane) probe)]
    (some (fn [^js s]
            (when (> (+ (.-offsetTop s) (.-offsetHeight s)) y)
              (parse-long (.. s -dataset -page))))
          (js/Array.from (.querySelectorAll pane ".doc-sheet")))))

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
