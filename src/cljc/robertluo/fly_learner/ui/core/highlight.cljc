(ns robertluo.fly-learner.ui.core.highlight
  "Find a node's source text on a PDF page.

  pdf.js gives a page as text items (runs of glyphs, often split inside
  formulas); the knowledge base holds the same text as extracted by PDFBox
  at ingest. Both are compared folded: NFKC, lowercase, letters and digits
  only. The needle is searched in chunks, so formula parts that the two
  extractors render differently only leave gaps, which are filled when the
  chunks around them match in order."
  (:require [clojure.string :as str]))

;;=============================================================================
;; Folding
;;=============================================================================

(defn- nfkc [^String s]
  #?(:clj  (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFKC)
     :cljs (.normalize s "NFKC")))

#?(:cljs (def ^:private alnum-re (js/RegExp. "[\\p{L}\\p{N}]" "u")))

(defn- alnum? [c]
  #?(:clj  (Character/isLetterOrDigit (char c))
     :cljs (.test alnum-re c)))

(defn- fold-cp
  "Folded form of one code point (a string), possibly empty or several chars."
  [cp]
  (apply str (filter alnum? (str/lower-case (nfkc cp)))))

(defn- code-points
  "[[offset code-point-string] ...] of s, offsets in UTF-16 units."
  [^String s]
  (loop [i 0 out []]
    (if (< i (count s))
      (let [cp  (.codePointAt s i)
            n   #?(:clj (Character/charCount cp) :cljs (if (> cp 0xFFFF) 2 1))]
        (recur (+ i n) (conj out [i (subs s i (+ i n))])))
      out)))

(defn fold [s] (apply str (map (comp fold-cp second) (code-points s))))

(defn- page-index
  "Fold the page's item strings into one string; `owners` maps each of its
  chars back to [item offset length] in the raw item strings."
  [strs]
  (let [pairs (for [[item s] (map-indexed vector strs)
                    [off cp] (code-points s)
                    c        (fold-cp cp)]
                [c [item off (count cp)]])]
    {:text   (apply str (map first pairs))
     :owners (mapv second pairs)}))

;;=============================================================================
;; Matching
;;=============================================================================

(def ^:private chunk-size 12)

(defn- chunks
  "[[offset chunk] ...]: needle in runs of chunk-size, the last run longer."
  [needle]
  (let [len (count needle)
        q   (max 1 (quot len chunk-size))]
    (for [i (range q)
          :let [s (* i chunk-size)
                e (if (= i (dec q)) len (+ s chunk-size))]]
      [s (subs needle s e)])))

(defn- index-of [^String s ^String sub from]
  #?(:clj  (.indexOf s sub (int from))
     :cljs (.indexOf s sub from)))

(defn- extend-run
  "Grow run {:a :b :start :end} char by char while text and needle agree."
  [^String text ^String needle {:keys [a b start end]}]
  (let [a (loop [a a s start]
                    (if (and (pos? a) (pos? s) (= (.charAt text (dec a)) (.charAt needle (dec s))))
                      (recur (dec a) (dec s))
                      a))
        b (loop [b b e end]
            (if (and (< b (count text)) (< e (count needle)) (= (.charAt text b) (.charAt needle e)))
              (recur (inc b) (inc e))
              b))]
    [a b]))

(defn find-ranges
  "Char ranges [[from to) ...] of folded `text` covered by folded `needle`.
  Chunks are matched in order; two matched chunks join into one run when the
  page text between them is not much longer than the needle text they skip.
  A run needs two chunks (unless the needle is one), then grows at both ends
  while the text still agrees. A one-chunk run that the next match does not
  continue is taken as a chance match and dropped, and the search resumes
  from the last real run, so it cannot skip past the text."
  [text needle]
  (let [cs      (chunks needle)
        need    (min 2 (count cs))
        real?   (fn [run] (and run (>= (:n run) need)))
        close   (fn [out run] (cond-> out (real? run) (conj (extend-run text needle run))))
        new-run (fn [i off c] {:a i :b (+ i (count c)) :start off :end (+ off (count c)) :n 1})]
    ;; base: where the search resumes after the last real run
    (loop [cs cs base 0 run nil out []]
      (if-let [[off c] (first cs)]
        (let [i (index-of text c (if run (:b run) base))]
          (cond
            (and run (>= i 0) (<= (- i (:b run)) (+ chunk-size (* 2 (- off (:end run))))))
            (recur (rest cs) base
                   (-> run (assoc :b (+ i (count c)) :end (+ off (count c))) (update :n inc)) out)

            (neg? i) (recur (rest cs) base run out)

            (real? run)
            (recur (rest cs) (:b run) (new-run i off c) (close out run))

            :else ; no run, or a chance match: look again from base
            (let [i (index-of text c base)]
              (recur (rest cs) base (when (>= i 0) (new-run i off c)) out))))
        (close out run)))))

(def ^:private min-needle 6)

(defn find-spans
  "Where `texts` occur among a page's text items `strs`. One vector per
  matched run, of spans {:item i :from x :to y} (offsets into (strs i))."
  [strs texts]
  (let [{:keys [text owners]} (page-index strs)]
    (vec (for [needle (map fold texts)
               :when  (>= (count needle) min-needle)
               [a b]  (find-ranges text needle)]
           (->> (subvec owners a b)
                (partition-by first)
                (mapv (fn [os]
                        (let [[item off] (first os)
                              [_ off' n] (last os)]
                          {:item item :from off :to (+ off' n)}))))))))

;;=============================================================================
;; Geometry
;;=============================================================================

(defn merge-rects
  "Join consecutive rects on the same line ({:x :y :w :h}, top-left origin),
  so a highlighted line is one box rather than one per text item."
  [rects]
  (reduce (fn [out {:keys [x y w h] :as r}]
            (let [{px :x py :y pw :w ph :h :as p} (peek out)
                  overlap (when p (- (min (+ y h) (+ py ph)) (max y py)))]
              (if (and p
                       (>= overlap (* 0.5 (min h ph)))
                       (>= x px)
                       (<= x (+ px pw (* 1.5 (max h ph)))))
                (let [x1 (max (+ x w) (+ px pw))
                      y0 (min y py)
                      y1 (max (+ y h) (+ py ph))]
                  (conj (pop out) {:x px :y y0 :w (- x1 px) :h (- y1 y0)}))
                (conj out r))))
          []
          rects))

^:rct/test
(comment
  (fold "Ｆinal ﬁx: 𝑥 ∈ FV(0 → 0)") ;=> "finalfixxfv00"

  ;; items split as pdf.js does; the needle as PDFBox extracted it
  (def items ["Note that" " " "x, w" "x" " " "/" "∈" " " "F V"
              ", thus by Lemma 2.1, Lemma 3.16, and computation"
              "rules of substitution this matches the previous right-hand side."
              "70"])
  (def para (str "Note that x, wx ∉ FV(0 → 0), thus by Lemma 2.1, Lemma 3.16, and "
                 "computation rules of substitution this matches the previous right-hand side."))

  ;; the formula differs ("FV(0 → 0)" is missing on the page), the run still spans it
  (map (juxt :item :from :to) (first (find-spans items [para])))
  ;=> [[0 0 9] [2 0 4] [3 0 1] [8 0 3] [9 2 48] [10 0 63]]

  ;; the page number is not part of it; unrelated or tiny needles match nothing
  (find-spans items ["Something else entirely, not on this page at all."]) ;=> []
  (find-spans items ["x"]) ;=> []

  ;; a paragraph continued from the previous page matches its tail
  (count (find-spans items [(str "A long first half that was printed on the page before this one, "
                                 "rules of substitution this matches the previous right-hand side.")]))
  ;=> 1

  ;; far apart matches stay separate runs
  (find-ranges (str "aaaaaaaaaaaaaaaaaaaaaaaa" (apply str (repeat 200 "z")) "bbbbbbbbbbbbbbbbbbbbbbbb")
               "aaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbb")
  ;=> [[0 24] [224 248]]

  ;; a subscript joins its line; the next line is a new box
  (merge-rects [{:x 10 :y 50 :w 10 :h 4} {:x 21 :y 52 :w 5 :h 3} {:x 10 :y 56 :w 20 :h 4}])
  ;=> [{:x 10 :y 50 :w 16 :h 5} {:x 10 :y 56 :w 20 :h 4}]
  )
