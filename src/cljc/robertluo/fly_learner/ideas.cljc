(ns robertluo.fly-learner.ideas
  "Ideas: the learner's own voice on what they study. An idea takes a
  stance on something (agree, restate it in their own words, disagree),
  adds a note to it, or just marks it: worth keeping (a highlight), or where to go on reading (a bookmark),
  as in any reading app. It may link
  to other ideas, so the learner's ideas form a graph of their own over
  the knowledge.

  This namespace depends on nothing. What an idea points at is an anchor,
  a plain map opaque here; an `Anchors` implementation (e.g.
  robertluo.fly-learner.ideas.knowledge) says what an anchor points at now.

    {:idea/id      uuid
     :idea/stance  :agree | :restate | :disagree | :note | :highlight | :bookmark
     :idea/text    Markdown (required for a note or a restatement, optional
                   otherwise); `[[a term]]` (or `[[a term|shown]]`) links to
                   what the term names, `wikilinks`; `[[A]] needs [[B]]`
                   states a prerequisite, `claims`
     :idea/anchors [anchor ..]   what it is about (one or more)
     :idea/links   [idea-id ..]  other ideas it relates to
     :idea/at      epoch ms}

  Text quotes (`quote-selector`, `find-quote`) anchor ideas to a span of
  text so that they can be found again when the text around them changes:
  the quote with a little context on both sides, as W3C Web Annotation's
  TextQuoteSelector.

  What the learner writes is theirs: it never moves their scores (learning
  knows nothing of ideas)."
  (:require [clojure.string :as str]))

(def stances #{:agree :restate :disagree :note :highlight :bookmark})

(def ^:private worded
  "Stances that are words: nothing without them."
  #{:note :restate})

(defprotocol Anchors
  (target [a anchor]
    "What `anchor` points at now, as a key ideas are grouped by (compared
     with =), or nil when it is gone.")
  (describe [a key] "{:kind kw :title str} of a target key, for display."))

(def max-text 5000)

(defn idea
  "A new idea from `{:stance :text :anchors :links}`, or {:error message}."
  [{:keys [stance text anchors links]} id at]
  (let [text (some-> text str)]
    (cond
      (not (stances stance))                    {:error "Agree, restate, disagree or note"}
      (and (worded stance) (empty? (some-> text str/trim)))
      {:error (if (= :restate stance) "Say it in your own words" "A note needs some words")}
      (> (count text) max-text)                 {:error (str "Up to " max-text " characters")}
      (not (and (sequential? anchors) (seq anchors) (every? map? anchors)))
      {:error "What is it about?"}
      :else (cond-> {:idea/id id :idea/stance stance :idea/anchors (vec anchors)
                     :idea/links (vec (distinct links)) :idea/at at}
              (seq (some-> text str/trim)) (assoc :idea/text (str/trim text))))))

(defn targets
  "Keys of what `idea`'s anchors point at now (gone ones left out)."
  [anchors idea]
  (into #{} (keep #(target anchors %)) (:idea/anchors idea)))

(defn stance-counts
  "{stance n} over `ideas`."
  [ideas]
  (frequencies (map :idea/stance ideas)))

(defn links
  "Undirected links between `ideas` (both ends among them): #{#{a b} ..}."
  [ideas]
  (let [ids (set (map :idea/id ideas))]
    (into #{} (for [{:idea/keys [id links]} ideas, l links :when (and (ids l) (not= l id))] #{id l}))))

;;=============================================================================
;; Links in what the learner writes
;;=============================================================================

(defn- without-code
  "Markdown `s` with its code (fenced blocks, spans) blanked: no links there."
  [s]
  (-> s
      (str/replace #"(?s)```.*?(```|$)" "")
      (str/replace #"`[^`\n]*`" "")))

(defn wikilink
  "[target shown] of the inside of a `[[..]]` link (`a term|shown`)."
  [inside]
  (let [[t shown] (str/split (str inside) #"\|" 2)
        t (str/trim t)]
    [t (if (str/blank? shown) t (str/trim shown))]))

(defn wikilinks
  "The terms `[[..]]` links in Markdown `text` point at, in order, once each."
  [text]
  (->> (re-seq #"\[\[([^\[\]\n]+)\]\]" (without-code (or text "")))
       (map (comp first wikilink second))
       (remove str/blank?)
       distinct
       vec))

(defn term-key
  "What a term written in `[[..]]` is grouped by: any case, any spacing."
  [t]
  (-> (str t) str/trim str/lower-case (str/replace #"\s+" " ")))

(def ^:private link-re "\\[\\[([^\\[\\]\\n]+)\\]\\]")

(def ^:private needs-re
  "`[[A]] needs [[B]]` (requires, depends on, builds on, assumes), `[[B]]
  (comes) before [[A]]`; more needed ones may follow, by commas or and."
  (re-pattern (str link-re "\\s+(needs|requires|depends on|builds on|assumes|(?:comes |is learned )?before)\\s+"
                   "(" link-re "(?:\\s*(?:,|,?\\s+and)\\s+" link-re ")*)")))

(defn claims
  "Prerequisites stated in Markdown `text`: [[of needs] ..] terms, `[[A]]
  needs [[B]]` (or requires, depends on, builds on, assumes; `[[B]] before
  [[A]]` the other way round). Once each, none in code."
  [text]
  (->> (re-seq needs-re (without-code (or text "")))
       (mapcat (fn [[_ a verb more]]
                 (let [a  (first (wikilink a))
                       bs (map (comp first wikilink second) (re-seq (re-pattern link-re) more))]
                   (for [b bs]
                     (if (str/includes? verb "before") [b a] [a b])))))
       (remove (fn [[a b]] (or (str/blank? a) (str/blank? b) (= (term-key a) (term-key b)))))
       distinct
       vec))

;;=============================================================================
;; Text quotes
;;=============================================================================

(def context-length 32)

(defn- whitespace? [c]
  (contains? #{\space \newline \tab \return (char 160)} c))

(defn- squeeze
  "`s` with runs of whitespace as one space, and for each index of the result
  the index in `s` it came from."
  [s]
  (loop [i 0, out (transient []), idx (transient []), space? false]
    (if (< i (count s))
      (let [c (nth s i), ws? (whitespace? c)]
        (cond
          (and ws? space?) (recur (inc i) out idx true)
          ws?              (recur (inc i) (conj! out \space) (conj! idx i) true)
          :else            (recur (inc i) (conj! out c) (conj! idx i) false)))
      [(apply str (persistent! out)) (persistent! idx)])))

(defn quote-selector
  "Selector for `text` [start, end): the quote, whitespace squeezed, and up to
  `context-length` characters on each side."
  [text start end]
  (let [sq #(first (squeeze %))]
    {:quote  (str/trim (sq (subs text start end)))
     :prefix (sq (subs text (max 0 (- start context-length)) start))
     :suffix (sq (subs text end (min (count text) (+ end context-length))))}))

(defn- index-of [^String s ^String q from]
  #?(:clj  (let [i (.indexOf s q (int from))] (when (>= i 0) i))
     :cljs (let [i (.indexOf s q from)] (when (>= i 0) i))))

(defn- common-suffix [a b]
  (count (take-while true? (map = (reverse a) (reverse b)))))

(defn- common-prefix [a b]
  (count (take-while true? (map = a b))))

(defn find-quote
  "[start end) in `text` of the quote `selector` names, or nil. Whitespace
  may differ; among several occurrences, the one whose surroundings best
  match the selector's prefix and suffix wins."
  [text {:keys [quote prefix suffix]}]
  (let [[t idx] (squeeze (or text ""))
        q       (str/trim (first (squeeze (or quote ""))))]
    (when (seq q)
      (let [hits (loop [from 0, acc []]
                   (if-let [i (index-of t q from)] (recur (inc i) (conj acc i)) acc))
            fit  (fn [i] (+ (common-suffix (subs t (max 0 (- i (count prefix))) i) (or prefix ""))
                            (common-prefix (subs t (+ i (count q))
                                                 (min (count t) (+ i (count q) (count suffix))))
                                           (or suffix ""))))]
        (when-let [i (first (sort-by (comp - fit) hits))]
          (let [j (+ i (count q))]
            [(idx i) (inc (idx (dec j)))]))))))

(defn mark-segments
  "`text` cut into [[s nil] [s mark] ..] where `marks` ({:selector .. :mark ..})
  are found; overlapping marks after the first are dropped."
  [text marks]
  (let [spans (->> marks
                   (keep (fn [{:keys [selector mark]}]
                           (when-let [[s e] (find-quote text selector)] [s e mark])))
                   (sort-by first)
                   (reduce (fn [acc [s e m]]
                             (if (and (seq acc) (< s (second (peek acc)))) acc (conj acc [s e m])))
                           []))]
    (loop [pos 0, [[s e m] & more] spans, out []]
      (if s
        (recur e more (cond-> out (< pos s) (conj [(subs text pos s) nil]) true (conj [(subs text s e) m])))
        (cond-> out (< pos (count text)) (conj [(subs text pos) nil]))))))

^:rct/test
(comment
  ;; a note needs words; a stance does not
  (:error (idea {:stance :note :text "  " :anchors [{:a 1}]} 1 0)) ;=> "A note needs some words"
  (:error (idea {:stance :restate :anchors [{:a 1}]} 1 0)) ;=> "Say it in your own words"
  (:idea/text (idea {:stance :restate :text " In short, [[closure]]s. " :anchors [{:a 1}]} 1 0))
  ;=> "In short, [[closure]]s."
  (idea {:stance :agree :anchors [{:a 1}] :links [7 7]} 1 0)
  ;=> {:idea/id 1 :idea/stance :agree :idea/anchors [{:a 1}] :idea/links [7] :idea/at 0}
  (:error (idea {:stance :agree :anchors []} 1 0)) ;=> "What is it about?"
  ;; a highlight is a mark, no words needed
  (:idea/stance (idea {:stance :highlight :anchors [{:quote "x"}]} 1 0)) ;=> :highlight

  (def anchors (reify Anchors
                 (target [_ a] (:to a))
                 (describe [_ k] {:kind :thing :title (str k)})))
  (targets anchors {:idea/anchors [{:to :x} {:to nil} {:to :y}]}) ;=> #{:x :y}
  (links [{:idea/id 1 :idea/links [2 9]} {:idea/id 2 :idea/links [1]}]) ;=> #{#{1 2}}

  ;; a quote is found again though whitespace changed; context picks the occurrence
  (def text "Erasure maps a proof to a term. Later,  erasure maps a\nproof to an object.")
  (def sel (quote-selector text 40 73))
  (:quote sel) ;=> "erasure maps a proof to an object"
  (find-quote "Erasure maps a proof to a term. Later, erasure maps a proof to an object." sel) ;=> [39 72]
  (find-quote "Erasure maps a proof to a term." sel) ;=> nil
  ;; the same quote twice: the prefix decides
  (find-quote "x a b. y a b." {:quote "a b" :prefix "y "}) ;=> [9 12]

  (mark-segments "one two three" [{:selector {:quote "two"} :mark :m}])
  ;=> [["one " nil] ["two" :m] [" three" nil]]
  (mark-segments "abc" [{:selector {:quote "zz"} :mark :m}]) ;=> [["abc" nil]]

  ;; links: not in code, once each, the shown text apart
  (wikilinks "A [[closure|closures]] needs [[ lambda ]], `[[x]]`, [[closure]]\n```\n[[y]]\n```")
  ;=> ["closure" "lambda"]
  (wikilinks nil) ;=> []
  (wikilink "closure|closures") ;=> ["closure" "closures"]
  (wikilink "lambda") ;=> ["lambda" "lambda"]

  ;; prerequisites stated: needs and its kin, several at once, before the other way round
  (claims "[[Streams]] need nothing, but [[stream|streams]] needs [[delay]], [[force]] and [[cons-stream]].")
  ;=> [["stream" "delay"] ["stream" "force"] ["stream" "cons-stream"]]
  (claims "[[closure]] builds on [[environment]]; [[lambda]] comes before [[closure]]. `[[a]] needs [[b]]`")
  ;=> [["closure" "environment"] ["closure" "lambda"]]
  (claims "[[x]] does not need [[y]]; [[x]] needs [[X]]") ;=> []
  (term-key " Lambda   Calculus ") ;=> "lambda calculus"
  )
