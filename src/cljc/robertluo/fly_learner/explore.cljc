(ns robertluo.fly-learner.explore
  "Explorations: an interactive view of some knowledge, as data. A spec says
  what state there is, what is derived from it and what is shown; this
  namespace evaluates it (pure, so the server can run a spec before keeping
  it) and a renderer draws what `view` returns. It depends on nothing.

    {:title  str
     :intro  template
     :cites  [node-id ..]          [n] in a template cites the n-th
     :vars   [{:name str :init expr} ..]   state the learner changes
     :derive [{:name str :expr expr} ..]   computed, in order, from the state
     :view   [element ..]}

  Every element may have `:show expr` (shown when truthy). Elements:

    :text     {:text template}
    :formula  {:text template :mark template}  monospace, `mark` highlighted
    :slider   {:var :min :max :step :label}
    :choice   {:var :label :options [{:label template :value expr} ..]}
    :toggle   {:var :label}
    :button   {:label :set [{:var :expr} ..]}
    :table    {:rows [template] :cols [template] :cell expr | :cells [[template]]
               :highlight expr :on-click [set ..] :caption template}
               (row, col and value are bound in cell, highlight, on-click)
    :plot     {:x-min :x-max :y-min :y-max (numbers; y auto when absent)
               :x-label :y-label :series [{:y expr :label} ..] (x is bound)
               :points [{:x expr :y expr :label} ..]}
    :diagram  {:nodes [{:id str :label template :highlight expr :show expr} ..]
               :edges [{:from :to :label template :highlight expr} ..]
               :on-click [set ..] :direction \"down\" | \"right\"}
               (id is bound for nodes and clicks; from, to for edges)
    :steps    {:var :frames [{:caption template :set [set ..]} ..]}
               frame i is the state with the sets of frames 0..i applied
    :predict  {:var :prompt template :options [{:label template} ..] :answer expr
               :explain template}  guess first, then the answer (option index)

  A template is text with expressions in braces: \"T = {t}\" ({{ is a brace,
  for sets: \"{{0, 1}\" reads {0, 1}). An expression
  is a string holding one s-expression (or the form itself): numbers,
  strings, keywords, vectors, maps; if when cond let fn and or; the
  `builtins`. Evaluation is bounded (fuel, call depth, sequence length), so
  any spec ends. A spec reaches here as JSON from a model or as EDN: keys
  may be strings, types strings or keywords (`normalize`)."
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.tools.reader.edn :as edn])
            #?(:cljs [cljs.tools.reader.reader-types :as rt])))

;;=============================================================================
;; Expressions
;;=============================================================================

(def ^:private max-fuel 200000)
(def ^:private max-depth 64)
(def max-seq "Longest sequence an expression may build." 2000)

(defn- fail [msg & args] (throw (ex-info (apply str msg args) {:explore true})))

(defn- error-message [e]
  #?(:clj  (or (ex-message e) (.getName (class e)))
     :cljs (or (ex-message e) (str e))))

(def ^:private read-form
  (memoize
   (fn [s]
     (let [eof    (symbol "eof")
           r      #?(:clj (java.io.PushbackReader. (java.io.StringReader. s))
                     :cljs (rt/string-push-back-reader s))
           [a b]  (try [(edn/read {:eof eof} r) (edn/read {:eof eof} r)]
                       (catch #?(:clj Exception :cljs :default) e
                         (fail "Cannot read " (pr-str s) ": " (error-message e))))]
       (cond (= eof a) (fail "Empty expression")
             (= eof b) a
             :else     (fail "Expected one expression in " (pr-str s)))))))

(defn read-expr
  "The form of expression `x`: a string is read, anything else is the form."
  [x]
  (if (string? x) (read-form x) x))

(defn- num! [x what]
  (if (number? x) x (fail what " needs a number, got " (pr-str x))))

(defn- bounded! [coll]
  (if (> (count coll) max-seq) (fail "Sequence longer than " max-seq) coll))

(defn- seq! [x what]
  (cond (nil? x) [] (or (sequential? x) (string? x) (set? x) (map? x)) (bounded! (vec x))
        :else (fail what " needs a sequence, got " (pr-str x))))

(defn- eq
  "= with numbers equal by value (1 = 1.0 on the JVM too)."
  ([_] true)
  ([a b] (cond (and (number? a) (number? b)) (== a b)
               (and (sequential? a) (sequential? b))
               (and (= (count a) (count b)) (every? true? (map eq a b)))
               :else (= a b)))
  ([a b & more] (and (eq a b) (apply eq b more))))

(defn- div* [a & more]
  (let [r (reduce (fn [x y] (if (zero? (num! y "/")) (fail "Division by zero") (/ (num! x "/") y)))
                  (if (seq more) a 1)
                  (if (seq more) more [a]))]
    #?(:clj (if (ratio? r) (double r) r) :cljs r)))

(defn- index-of [coll x]
  (or (first (keep-indexed (fn [i y] (when (eq x y) i)) coll)) -1))

(defn- argmax [coll]
  (let [v (seq! coll "argmax")]
    (when (seq v) (reduce (fn [best i] (if (> (num! (nth v i) "argmax") (nth v best)) i best)) 0 (range (count v))))))

(defn- rng
  ([n] (rng 0 n 1))
  ([a b] (rng a b 1))
  ([a b step]
   (num! a "range") (num! b "range")
   (when (zero? (num! step "range")) (fail "range step 0"))
   (when (> (/ (- b a) step) max-seq) (fail "Sequence longer than " max-seq))
   (vec (range a b step))))

(defn- nth*
  ([coll i] (let [v (seq! coll "nth")]
              (if (and (integer? i) (< -1 i (count v)))
                (nth v i)
                (fail "nth: no index " (pr-str i) " in " (count v) " items"))))
  ([coll i not-found] (let [v (seq! coll "nth")]
                        (if (and (integer? i) (< -1 i (count v))) (nth v i) not-found))))

(declare format-value)

(def builtins
  "Functions an expression may call, by name."
  {"+" + "-" - "*" * "/" div* "mod" mod "quot" quot "rem" rem "inc" inc "dec" dec
   "abs" #(if (neg? %) (- %) %) "min" min "max" max
   "round" #(Math/round (double %)) "floor" #(Math/floor %) "ceil" #(Math/ceil %)
   "sqrt" #(Math/sqrt %) "pow" #(Math/pow %1 %2) "exp" #(Math/exp %) "log" #(Math/log %)
   "=" eq "not=" (complement eq) "<" < ">" > "<=" <= ">=" >=
   "not" not "nil?" nil? "some?" some? "number?" number? "string?" string? "zero?" zero?
   "pos?" pos? "neg?" neg? "even?" even? "odd?" odd?
   "count" #(count (seq! % "count")) "nth" nth* "get" get "get-in" get-in
   "first" first "second" second "last" #(peek (seq! % "last")) "rest" #(vec (rest (seq! % "rest")))
   "take" #(vec (take %1 (seq! %2 "take"))) "drop" #(vec (drop %1 (seq! %2 "drop")))
   "conj" (fn [coll & xs] (bounded! (apply conj (or coll []) xs)))
   "assoc" assoc "assoc-in" assoc-in "dissoc" dissoc "update" update "keys" #(vec (keys %)) "vals" #(vec (vals %))
   "concat" (fn [& cs] (bounded! (vec (mapcat #(seq! % "concat") cs))))
   "vec" #(seq! % "vec") "vector" vector "list" vector "hash-map" hash-map "set" #(set (seq! % "set"))
   "range" rng "repeat" (fn [n x] (when (> n max-seq) (fail "Sequence longer than " max-seq)) (vec (repeat n x)))
   "map" (fn [f & cs] (bounded! (apply mapv f (map #(seq! % "map") cs))))
   "map-indexed" (fn [f c] (vec (map-indexed f (seq! c "map-indexed"))))
   "filter" (fn [f c] (filterv f (seq! c "filter"))) "remove" (fn [f c] (vec (remove f (seq! c "remove"))))
   "reduce" (fn ([f c] (reduce f (seq! c "reduce"))) ([f init c] (reduce f init (seq! c "reduce"))))
   "some" (fn [f c] (some f (seq! c "some"))) "every?" (fn [f c] (every? f (seq! c "every?")))
   "apply" (fn [f & args] (apply f (concat (butlast args) (seq! (last args) "apply"))))
   "sort" #(vec (sort (seq! % "sort"))) "sort-by" (fn [f c] (vec (sort-by f (seq! c "sort-by"))))
   "reverse" #(vec (reverse (seq! % "reverse"))) "distinct" #(vec (distinct (seq! % "distinct")))
   "contains?" contains? "index-of" index-of "argmax" argmax
   "column" (fn [m j] (mapv #(nth % j) (seq! m "column")))
   "sum" #(reduce + 0 (seq! % "sum"))
   "str" (fn [& xs] (apply str (map format-value xs)))
   "subs" subs "upper-case" str/upper-case "lower-case" str/lower-case
   "includes?" str/includes? "join" (fn ([c] (str/join (map format-value c))) ([sep c] (str/join sep (map format-value c))))
   "fmt" (fn [x digits] (format-value (if (number? x) (let [k (Math/pow 10 digits)] (/ (Math/round (* x k)) k)) x)))})

(defn- burn! [{:keys [fuel]}]
  (when (neg? (vswap! fuel dec)) (fail "Too much computation")))

(declare ev)

(defn- bind
  "Bind `pattern` (a symbol, `_` or a vector of patterns) to `value` in locals."
  [locals pattern value]
  (cond
    (= '_ pattern)     locals
    (symbol? pattern)  (assoc locals (name pattern) value)
    (vector? pattern)  (let [v (seq! value "destructuring")]
                         (reduce (fn [l [i p]] (bind l p (get v i))) locals (map-indexed vector pattern)))
    :else (fail "Cannot bind " (pr-str pattern))))

(defn- ev-body [ctx locals body]
  (reduce (fn [_ f] (ev ctx locals f)) nil body))

(defn- closure [ctx locals params body]
  (when-not (vector? params) (fail "fn needs a parameter vector"))
  (fn [& args]
    (when (not= (count params) (count args))
      (fail "fn takes " (count params) " arguments, got " (count args)))
    (when (> (vswap! (:depth ctx) inc) max-depth) (fail "Calls nested too deep"))
    (try (ev-body ctx (reduce (fn [l [p a]] (bind l p a)) locals (map vector params args)) body)
         (finally (vswap! (:depth ctx) dec)))))

(defn- ev-list [ctx locals [head & args :as form]]
  (case (when (symbol? head) (name head))
    "if"   (let [[t a b] args] (if (ev ctx locals t) (ev ctx locals a) (ev ctx locals b)))
    "when" (when (ev ctx locals (first args)) (ev-body ctx locals (rest args)))
    "cond" (loop [[t e & more] args]
             (cond (nil? t) nil
                   (ev ctx locals t) (ev ctx locals e)
                   :else (recur more)))
    "and"  (reduce (fn [_ f] (let [v (ev ctx locals f)] (if v v (reduced v)))) true args)
    "or"   (reduce (fn [_ f] (let [v (ev ctx locals f)] (if v (reduced v) v))) nil args)
    "let"  (let [[bindings & body] args]
             (when-not (and (vector? bindings) (even? (count bindings))) (fail "let needs [name value ..]"))
             (ev-body ctx (reduce (fn [l [p e]] (bind l p (ev ctx l e))) locals (partition 2 bindings)) body))
    "fn"   (closure ctx locals (first args) (rest args))
    (if (nil? head)
      (fail "Empty call ()")
      (let [f (ev ctx locals head)]
        (when-not (and (ifn? f) (not (symbol? f))) (fail (pr-str head) " is not a function in " (pr-str form)))
        (apply f (mapv #(ev ctx locals %) args))))))

(defn- ev [ctx locals form]
  (burn! ctx)
  (cond
    (symbol? form) (let [n (name form)]
                     (cond (namespace form) (fail "Unknown name " form)
                           (contains? locals n) (get locals n)
                           (contains? builtins n) (builtins n)
                           :else (fail "Unknown name " n)))
    (seq? form)    (ev-list ctx locals form)
    (vector? form) (mapv #(ev ctx locals %) form)
    (map? form)    (into {} (map (fn [[k v]] [(ev ctx locals k) (ev ctx locals v)])) form)
    (set? form)    (into #{} (map #(ev ctx locals %)) form)
    :else          form))

(defn- context [] {:fuel (volatile! max-fuel) :depth (volatile! 0)})

(defn evaluate
  "Value of expression `x` with `env` (name -> value; names are strings)."
  ([env x] (evaluate (context) env x))
  ([ctx env x] (ev ctx env (read-expr x))))

;;=============================================================================
;; Values as text, templates
;;=============================================================================

(defn- format-number [x]
  (cond
    #?(:clj (Double/isNaN (double x)) :cljs (js/isNaN x)) "NaN"
    #?(:clj (Double/isInfinite (double x)) :cljs (not (js/isFinite x))) (if (pos? x) "∞" "−∞")
    (and (== x (Math/round (double x))) (< (Math/abs (double x)) 1e15)) (str (Math/round (double x)))
    :else (let [r (/ (Math/round (* (double x) 1000.0)) 1000.0)]
            (if (zero? r) (str x) (str r)))))

(defn format-value
  "How a value reads in a template."
  [x]
  (cond
    (nil? x)        ""
    (string? x)     x
    (number? x)     (format-number x)
    (keyword? x)    (name x)
    (map? x)        (str "{" (str/join ", " (map (fn [[k v]] (str (format-value k) " " (format-value v))) x)) "}")
    (sequential? x) (str "(" (str/join ", " (map format-value x)) ")")
    (set? x)        (str "{" (str/join ", " (map format-value x)) "}")
    :else           (str x)))

(defn template-parts
  "`s` as [[:text str] | [:expr str] ..]: expressions are in braces (nested
  braces and braces inside strings allowed); an unclosed brace is text, {{
  a literal brace."
  [s]
  (let [s (or s "") n (count s)]
    (loop [i 0, start 0, out []]
      (let [open (str/index-of s "{" i)]
        (cond
          (nil? open)
          (cond-> out (< start n) (conj [:text (subs s start)]))

          (and (< (inc open) n) (= \{ (nth s (inc open))))
          (recur (+ 2 open) (+ 2 open) (conj out [:text (str (subs s start open) "{")]))

          :else
          (let [close (loop [j (inc open), depth 1, in-str? false]
                        (when (< j n)
                          (let [c (nth s j)]
                            (cond
                              (and in-str? (= c \\))  (recur (+ j 2) depth true)
                              (= c \")                (recur (inc j) depth (not in-str?))
                              in-str?                 (recur (inc j) depth true)
                              (= c \{)                (recur (inc j) (inc depth) false)
                              (= c \})                (if (= 1 depth) j (recur (inc j) (dec depth) false))
                              :else                   (recur (inc j) depth false)))))]
            (if (nil? close)
              (conj out [:text (subs s start)])
              (recur (inc close) (inc close)
                     (cond-> out
                       (< start open) (conj [:text (subs s start open)])
                       true           (conj [:expr (subs s (inc open) close)]))))))))))

(defn fill
  "Template `s` with its expressions' values."
  ([env s] (fill (context) env s))
  ([ctx env s]
   (apply str (for [[k x] (template-parts s)]
                (if (= :text k) x (format-value (evaluate ctx env x)))))))

(defn citations
  "The [n] citations in `s`, in order."
  [s]
  (mapv (comp parse-long second) (re-seq #"\[(\d+)\]" (or s ""))))

(defn- failure
  "The message `x` fails with in `env`, or nil (for tests)."
  [env x]
  (try (evaluate env x) nil (catch #?(:clj Exception :cljs :default) e (ex-message e))))

^:rct/test
(comment
  (evaluate {"t" 7 "r" 3} "(if (> t r) :defect :cooperate)") ;=> :defect
  (evaluate {} "(let [[a b] [1 2]] (+ a b))") ;=> 3
  (evaluate {} "(map (fn [x] (* x x)) (range 4))") ;=> [0 1 4 9]
  (evaluate {} "(= [0 1] [0 1.0])") ;=> true
  (evaluate {} "(/ 1 4)") ;=> 0.25
  (evaluate {"A" [[3 0] [5 1]]} "(apply max (column A 0))") ;=> 5
  (evaluate {} "(argmax [1 5 2])") ;=> 1
  ;; any spec ends: runaway recursion runs out of depth, loops out of fuel
  (failure {} "((fn [f] (f f)) (fn [f] (f f)))")
  ;=> "Calls nested too deep"
  (failure {} "(range 1e9)")
  ;=> "Sequence longer than 2000"
  (failure {} "(foo 1)")
  ;=> "Unknown name foo"
  (failure {} "1 2")
  ;=> "Expected one expression in \"1 2\""
  (template-parts "{{0, 1} has {n}")
  ;=> [[:text "{"] [:text "0, 1} has "] [:expr "n"]]
  (template-parts "T = {t}, {(str \"}\" {:a 1})} {open")
  ;=> [[:text "T = "] [:expr "t"] [:text ", "] [:expr "(str \"}\" {:a 1})"] [:text " {open"]]
  (fill {"t" 7 "p" [1 2.5]} "T = {t}, payoffs {p}, {(/ 2 3)}") ;=> "T = 7, payoffs (1, 2.5), 0.667"
  (citations "As shown [1][3], not [x].") ;=> [1 3]
  )

;;=============================================================================
;; Specs
;;=============================================================================

(defn cited-numbers
  "The [n] cited anywhere in `spec`'s text (intro, element texts, captions,
  prompts, explanations), sorted."
  [spec]
  (let [texts (atom [(:intro spec)])]
    (doseq [el (:view spec)]
      (swap! texts into (map el [:text :caption :prompt :explain]))
      (swap! texts into (map :caption (:frames el))))
    (vec (sort (distinct (mapcat citations @texts))))))

(def element-types
  #{:text :formula :slider :choice :toggle :button :table :plot :diagram :steps :predict})

(defn- keywordize [x]
  (cond
    (map? x)        (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) (keywordize v)])) x)
    (sequential? x) (mapv keywordize x)
    :else           x))

(defn normalize
  "A spec from JSON or EDN: keyword keys, keyword element types."
  [spec]
  (-> (keywordize spec)
      (update :view (fn [els] (mapv #(update % :type (fn [t] (some-> t name keyword))) els)))))

(defn- var-name [x] (some-> x name))

(defn- sets!
  "Apply set actions `sets` ([{:var :expr}]) to `state`, in order, each seeing
  the ones before it; `env` is the rest of what expressions see."
  [ctx env state sets]
  (reduce (fn [st {v :var e :expr}]
            (assoc st (var-name v) (evaluate ctx (merge env st) e)))
          state sets))

(defn- clamp [x lo hi] (max lo (min hi x)))

(defn- frame-index [el state]
  (let [n (count (:frames el)) i (get state (var-name (:var el)))]
    (if (and (number? i) (pos? n)) (clamp (long i) 0 (dec n)) 0)))

(defn- derived
  "`state` with the spec's derived values, in order."
  [ctx spec state]
  (reduce (fn [m {n :name e :expr}]
            (assoc m (var-name n) (try (evaluate ctx m e)
                                       (catch #?(:clj Exception :cljs :default) ex
                                         (fail "derive " (var-name n) ": " (error-message ex))))))
          state (:derive spec)))

(defn env
  "What expressions see in `state`: for each steps element, the sets of its
  frames up to the current one applied (each seeing the values derived so
  far), then the derived values. Throws when a frame or a derived value
  fails."
  [spec state]
  (let [ctx   (context)
        state (reduce (fn [st [i el]]
                        (if (= :steps (:type el))
                          (reduce (fn [st [j frame]]
                                    (try (sets! ctx (apply dissoc (derived ctx spec st) (keys st)) st (:set frame))
                                         (catch #?(:clj Exception :cljs :default) ex
                                           (fail "view[" i "] frame " j ": " (error-message ex)))))
                                  st (map-indexed vector (take (inc (frame-index el st)) (:frames el))))
                          st))
                      state (map-indexed vector (:view spec)))]
    (derived ctx spec state)))

(defn- implicit-init [ctx state el]
  (let [v (var-name (:var el))]
    (if (or (nil? v) (contains? state v))
      state
      (case (:type el)
        :slider  (assoc state v (:min el 0))
        :toggle  (assoc state v false)
        :choice  (assoc state v (some->> el :options first :value (evaluate ctx state)))
        :steps   (assoc state v 0)
        :predict (assoc state v nil)
        state))))

(defn init-state
  "The state a spec starts in: its vars, in order, then a start for each
  input's var it does not declare."
  [spec]
  (let [ctx (context)
        st  (reduce (fn [st {n :name e :init}] (assoc st (var-name n) (evaluate ctx st e))) {} (:vars spec))]
    (reduce #(implicit-init ctx %1 %2) st (:view spec))))

;;=============================================================================
;; View: elements with their values
;;=============================================================================

(defn- option-label [o] (if (map? o) (:label o) o))

(defn- shown? [ctx env el]
  (or (not (contains? el :show)) (boolean (evaluate ctx env (:show el)))))

(defn- plot-range [lo hi values]
  (let [vs (filter #(and (number? %) #?(:clj (Double/isFinite (double %)) :cljs (js/isFinite %))) values)]
    (if (and (some? lo) (some? hi))
      [lo hi]
      (let [mn (if (seq vs) (apply min vs) 0) mx (if (seq vs) (apply max vs) 1)
            pad (if (== mn mx) 1 (* 0.05 (- mx mn)))]
        [(or lo (- mn pad)) (or hi (+ mx pad))]))))

(def ^:private plot-samples 101)

(defn- resolve-element [ctx env {:keys [type] :as el}]
  (let [fill* #(fill ctx env %)
        base  (select-keys el [:type :index])
        ev*   (fn [locals x] (evaluate ctx (merge env locals) x))]
    (case type
      :text    (assoc base :text (fill* (:text el)))
      :formula (assoc base :text (fill* (:text el)) :mark (some-> (:mark el) fill*))
      :slider  (assoc base :label (fill* (:label el)) :min (:min el 0) :max (:max el 1) :step (:step el 1)
                      :value (get env (var-name (:var el))))
      :toggle  (assoc base :label (fill* (:label el)) :value (boolean (get env (var-name (:var el)))))
      :choice  (let [cur (get env (var-name (:var el)))]
                 (assoc base :label (fill* (:label el))
                        :options (mapv (fn [{:keys [label value]}]
                                         {:label (fill* label) :selected? (eq cur (ev* {} value))})
                                       (:options el))))
      :button  (assoc base :label (fill* (:label el)))
      :table   (let [rows (mapv fill* (:rows el)) cols (mapv fill* (:cols el))]
                 (assoc base :rows rows :cols cols :caption (some-> (:caption el) fill*)
                        :clickable? (boolean (seq (:on-click el)))
                        :cells (vec (for [r (range (count rows))]
                                      (vec (for [c (range (count cols))]
                                             (let [loc   {"row" r "col" c}
                                                   value (if (contains? el :cell)
                                                           (ev* loc (:cell el))
                                                           (fill ctx (merge env loc) (get-in el [:cells r c])))
                                                   loc   (assoc loc "value" value)]
                                               {:text       (format-value value)
                                                :highlight? (and (contains? el :highlight)
                                                                 (boolean (ev* loc (:highlight el))))})))))))
      :plot    (let [x-min (:x-min el 0) x-max (:x-max el 1)
                     xs     (map #(+ x-min (* (- x-max x-min) (/ % (double (dec plot-samples))))) (range plot-samples))
                     series (mapv (fn [{:keys [y label]}]
                                    {:label  (some-> label fill*)
                                     :points (mapv (fn [x] (let [v (ev* {"x" x} y)] [x (when (number? v) v)])) xs)})
                                  (:series el))
                     points (mapv (fn [{:keys [x y label]}]
                                    {:x (ev* {} x) :y (ev* {} y) :label (some-> label fill*)})
                                  (:points el))
                     [y-min y-max] (plot-range (:y-min el) (:y-max el)
                                               (concat (mapcat #(map second (:points %)) series) (map :y points)))]
                 (assoc base :x-min x-min :x-max x-max :y-min y-min :y-max y-max
                        :x-label (some-> (:x-label el) fill*) :y-label (some-> (:y-label el) fill*)
                        :series series :points points))
      :diagram (let [nodes (into [] (comp (filter #(shown? ctx (assoc env "id" (:id %)) %))
                                          (map (fn [{:keys [id label highlight]}]
                                                 (let [loc {"id" id}]
                                                   {:id id :label (fill ctx (merge env loc) (or label id))
                                                    :highlight? (boolean (and highlight (ev* loc highlight)))}))))
                                 (:nodes el))
                     ids   (set (map :id nodes))]
                 (assoc base :direction (if (= "right" (some-> (:direction el) name)) :right :down)
                        :clickable? (boolean (seq (:on-click el))) :nodes nodes
                        :edges (into [] (comp (filter #(and (ids (:from %)) (ids (:to %))))
                                              (map (fn [{:keys [from to label highlight]}]
                                                     (let [loc {"from" from "to" to}]
                                                       {:from from :to to :label (some->> label (fill ctx (merge env loc)))
                                                        :highlight? (boolean (and highlight (ev* loc highlight)))}))))
                                     (:edges el))))
      :steps   (let [i (frame-index el env)]
                 (assoc base :at i :count (count (:frames el))
                        :caption (some-> (get-in el [:frames i :caption]) fill*)))
      :predict (let [chosen (get env (var-name (:var el)))
                     answer (ev* {} (:answer el))]
                 (cond-> (assoc base :prompt (fill* (:prompt el)) :options (mapv (comp fill* option-label) (:options el))
                                :chosen chosen)
                   (some? chosen) (assoc :answer answer :correct? (eq chosen answer)
                                         :explain (some-> (:explain el) fill*))))
      (fail "Unknown element type " (pr-str type)))))

(defn view
  "The elements of `spec` shown in `state`, with their values:
  [{:type .. :index i ..}], i the element's place in the spec's :view.
  An element that fails is {:type :error :index i :message str}."
  [spec state]
  (let [env (try (env spec state)
                 (catch #?(:clj Exception :cljs :default) e e))]
    (if-not (map? env)
      [{:type :error :index nil :message (error-message env)}]
      (into []
            (keep (fn [[i el]]
                    (let [ctx (context) el (assoc el :index i)]
                      (try (when (shown? ctx env el) (resolve-element ctx env el))
                           (catch #?(:clj Exception :cljs :default) e
                             {:type :error :index i :message (error-message e)})))))
            (map-indexed vector (:view spec))))))

;;=============================================================================
;; Actions
;;=============================================================================

(defn- act!
  [spec state {:keys [index value row col id step] :as action}]
  (if (= :reset action)
    (init-state spec)
    (let [el  (get-in spec [:view index])
          v   (var-name (:var el))
          ctx (context)
          env* (fn [] (env spec state))]
      (case (:type el)
        :slider  (let [x (clamp (num! value "slider") (:min el 0) (:max el 1))]
                   ;; whole numbers stay integers (indices, =)
                   (assoc state v (if (== x (Math/round (double x))) (Math/round (double x)) x)))
        :toggle  (assoc state v (boolean value))
        :choice  (assoc state v (evaluate ctx (env*) (get-in el [:options value :value])))
        :button  (sets! ctx (env*) state (:set el))
        :table   (let [loc {"row" row "col" col}
                       e   (env*)
                       value (if (contains? el :cell)
                               (evaluate ctx (merge e loc) (:cell el))
                               (fill ctx (merge e loc) (get-in el [:cells row col])))]
                   (sets! ctx (merge e loc {"value" value}) state (:on-click el)))
        :diagram (sets! ctx (merge (env*) {"id" id}) state (:on-click el))
        :steps   (assoc state v (clamp (long step) 0 (max 0 (dec (count (:frames el))))))
        :predict (if (some? (get state v)) state (assoc state v value))
        (fail "No action on " (pr-str action))))))

(defn act
  "The state after `action` on the element at `:index`:
    slider, toggle  {:index i :value v}
    choice, predict {:index i :value option-index}
    button          {:index i}
    table           {:index i :row r :col c}
    diagram         {:index i :id node-id}
    steps           {:index i :step n}
  or :reset (the initial state). An action that fails leaves it as it was."
  [spec state action]
  (try (act! spec state action)
       (catch #?(:clj Exception :cljs :default) _ state)))

;;=============================================================================
;; Checking a spec (before it is kept)
;;=============================================================================

(defn- shape-problems
  "What is missing or malformed in `spec`, as messages."
  [{:keys [title vars derive view cites] :as spec}]
  (let [declared (set (concat (map (comp var-name :name) vars) (map (comp var-name :name) derive)))
        el-problems
        (fn [i {:keys [type] :as el}]
          (let [at   (str "view[" i "] " (some-> type name) ": ")
                need (fn [& ks] (for [k ks :when (nil? (get el k))] (str at "needs " (name k))))]
            (concat
             (case type
               :text    (need :text)
               :formula (need :text)
               :slider  (concat (need :var :min :max)
                                (when (and (number? (:min el)) (number? (:max el)) (>= (:min el) (:max el)))
                                  [(str at "min must be below max")]))
               :toggle  (need :var)
               :choice  (concat (need :var :options) (when (empty? (:options el)) [(str at "needs options")]))
               :button  (need :label :set)
               :table   (concat (need :rows :cols)
                                (when-not (or (contains? el :cell) (contains? el :cells))
                                  [(str at "needs cell or cells")])
                                (when (and (:cells el)
                                           (or (not= (count (:cells el)) (count (:rows el)))
                                               (some #(not= (count %) (count (:cols el))) (:cells el))))
                                  [(str at "cells must be rows x cols")]))
               :plot    (concat (need :x-min :x-max)
                                (when (empty? (concat (:series el) (:points el))) [(str at "needs series or points")]))
               :diagram (let [ids (map :id (:nodes el))]
                          (concat (need :nodes)
                                  (when (not= (count ids) (count (set ids))) [(str at "node ids must be unique")])
                                  (for [{:keys [from to]} (:edges el)
                                        x [from to] :when (not (contains? (set ids) x))]
                                    (str at "edge end " (pr-str x) " is not a node"))))
               :steps   (concat (need :var :frames) (when (empty? (:frames el)) [(str at "needs frames")]))
               :predict (concat (need :var :prompt :options :answer)
                                (when (empty? (:options el)) [(str at "needs options")]))
               [(str "view[" i "]: unknown type " (pr-str type) ", one of "
                     (str/join ", " (map name (sort element-types))))])
             (when (and (:var el) (contains? (set (map (comp var-name :name) derive)) (var-name (:var el))))
               [(str at (var-name (:var el)) " is derived, not a var")]))))]
    (concat
     (when (str/blank? title) ["needs a title"])
     (when (empty? view) ["needs a view"])
     (when (not= (count declared) (+ (count vars) (count derive))) ["var and derive names must be unique"])
     (mapcat el-problems (range) view)
     (let [n (count cites)
           templates (concat [(:intro spec)]
                             (for [el view, k [:text :caption :explain :prompt]] (get el k))
                             (for [el view, f (:frames el)] (:caption f)))]
       (for [t templates, c (citations t) :when (not (<= 1 c n))]
         (str "citation [" c "] but " n " cites"))))))

(defn- expressions
  "Every expression and template of `spec`, as [where kind text]."
  [{:keys [vars derive view intro]}]
  (let [x (fn [w k v] (when (some? v) [[w k v]]))]
    (concat
     (x "intro" :template intro)
     (mapcat #(x (str "var " (var-name (:name %))) :expr (:init %)) vars)
     (mapcat #(x (str "derive " (var-name (:name %))) :expr (:expr %)) derive)
     (apply concat
            (for [[i el] (map-indexed vector view)
                  :let [w (str "view[" i "]")
                        sets (fn [w ss] (mapcat #(x (str w " set " (var-name (:var %))) :expr (:expr %)) ss))]]
              (concat
               (x w :expr (:show el))
               (mapcat #(x w :template (get el %)) [:text :mark :label :caption :prompt :explain])
               (mapcat #(x w :template %) (:rows el)) (mapcat #(x w :template %) (:cols el))
               (mapcat #(x w :template %) (flatten (:cells el)))
               (mapcat #(x w :template (option-label %)) (if (= :predict (:type el)) (:options el) []))
               (x w :expr (:cell el)) (x w :expr (:highlight el)) (x w :expr (:answer el))
               (sets w (:on-click el)) (sets w (:set el))
               (mapcat #(concat (x w :template (:label %)) (x w :expr (:value %))) (when (= :choice (:type el)) (:options el)))
               (mapcat #(concat (x w :expr (:y %)) (x w :expr (:x %)) (x w :template (:label %))) (concat (:series el) (:points el)))
               (mapcat #(concat (x w :template (:label %)) (x w :expr (:highlight %)) (x w :expr (:show %))) (:nodes el))
               (mapcat #(concat (x w :template (:label %)) (x w :expr (:highlight %))) (:edges el))
               (mapcat (fn [[j f]] (concat (x (str w " frame " j) :template (:caption f)) (sets (str w " frame " j) (:set f))))
                       (map-indexed vector (:frames el)))))))))

(defn- read-problems [spec]
  (for [[w kind s] (expressions spec)
        msg (try (if (= :expr kind)
                   (do (read-expr s) nil)
                   (doseq [[k e] (template-parts s) :when (= :expr k)] (read-expr e)))
                 nil
                 (catch #?(:clj Exception :cljs :default) e [(error-message e)]))]
    (str w ": " msg)))

(def ^:private max-runs 300)

(defn- states-to-try
  "States the learner can reach in one step from the initial one: each
  frame, each slider at its ends, each toggle, choice, button, click."
  [spec init]
  (let [els (map-indexed vector (:view spec))]
    (->> (concat
          [init]
          (for [[i el] els
                action (case (:type el)
                         :steps   (map #(hash-map :index i :step %) (range (count (:frames el))))
                         :slider  [{:index i :value (:min el 0)} {:index i :value (:max el 1)}]
                         :toggle  [{:index i :value true} {:index i :value false}]
                         :choice  (map #(hash-map :index i :value %) (range (count (:options el))))
                         :predict (map #(hash-map :index i :value %) (range (count (:options el))))
                         :button  [{:index i}]
                         :table   (for [r (range (count (:rows el))) c (range (count (:cols el)))
                                        :when (seq (:on-click el))]
                                    {:index i :row r :col c})
                         :diagram (for [n (:nodes el) :when (seq (:on-click el))] {:index i :id (:id n)})
                         [])]
            [action (try (act! spec init action)
                         (catch #?(:clj Exception :cljs :default) e e))]))
         (take max-runs))))

(defn check
  "Problems with `spec` (normalized), as messages; empty when it is sound:
  its shape, every expression readable, and run, from the initial state and
  each state one action away, without an error."
  [spec]
  (let [shape (shape-problems spec)
        reads (read-problems spec)]
    (if (seq (concat shape reads))
      (vec (concat shape reads))
      (let [init (try (init-state spec) (catch #?(:clj Exception :cljs :default) e e))]
        (if-not (map? init)
          [(str "initial state: " (error-message init))]
          (->> (states-to-try spec init)
               (mapcat (fn [x]
                         (let [[action st] (if (map? x) [nil x] x)
                               after (when action (str " after " (pr-str action)))]
                           (if-not (map? st)
                             [(str "action " (pr-str action) ": " (error-message st))]
                             (for [{:keys [type index message]} (view spec st) :when (= :error type)]
                               (str (if index (str "view[" index "]: ") "") message after))))))
               distinct
               vec))))))

^:rct/test
(comment
  (def spec (normalize {"title" "Doubling" "vars" [{"name" "n" "init" "1"}]
                        "derive" [{"name" "twice" "expr" "(* 2 n)"}]
                        "view" [{"type" "slider" "var" "n" "min" 0 "max" 10 "label" "n"}
                                {"type" "text" "text" "2 × {n} = {twice}"}
                                {"type" "text" "text" "big" "show" "(> n 5)"}]}))
  (check spec) ;=> []
  (init-state spec) ;=> {"n" 1}
  (view spec {"n" 3})
  ;=> [{:type :slider :index 0 :label "n" :min 0 :max 10 :step 1 :value 3}
  ;    {:type :text :index 1 :text "2 × 3 = 6"}]
  (count (view spec (act spec {"n" 3} {:index 0 :value 8}))) ;=> 3
  ;; the slider stays in its range; a failing action changes nothing
  (act spec {"n" 3} {:index 0 :value 99}) ;=> {"n" 10}
  (act spec {"n" 3} {:index 1}) ;=> {"n" 3}
  ;; an element that fails says why, the others still show
  (view (assoc-in spec [:view 1 :text] "{(nth [] n)}") {"n" 3})
  ;=> [{:type :slider :index 0 :label "n" :min 0 :max 10 :step 1 :value 3}
  ;    {:type :error :index 1 :message "nth: no index 3 in 0 items"}]
  ;; what a model must fix
  (check (normalize {"title" "" "view" [{"type" "chart"} {"type" "text" "text" "{(+ 1}"}
                                        {"type" "text" "text" "see [2]"}]}))
  ;=> ["needs a title" "view[0]: unknown type :chart, one of button, choice, diagram, formula, plot, predict, slider, steps, table, text, toggle" "citation [2] but 0 cites" "view[1]: Cannot read \"(+ 1\": EOF while reading"]
  (check (assoc-in spec [:view 1 :text] "{(/ 1 (- n 10))}"))
  ;=> ["view[1]: Division by zero after {:index 0, :value 10}"]
  )
