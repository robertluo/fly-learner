(ns robertluo.fly-learner.ui.core.views.markdown
  "Rich text: Markdown (GFM tables, lists, emphasis, code) with TeX formulas,
  $..$ inline and $$..$$ displayed, as Replicant hiccup. What the model
  writes (answers, questions, model answers, grading) comes in it.

  Citations [n] become what `:cite` makes of n (hiccup) when given, else
  stay text. Formulas are drawn by KaTeX in the browser; on the JVM they
  stay TeX. Requires nothing of the app."
  (:require [clojure.string :as str]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.transform :as md.transform]
            #?(:cljs ["katex" :as katex])))

(def ^:private cite-href "#cite-")

(defn- mark-citations
  "[n] (not a link already) as a link the renderer knows: Markdown leaves
  brackets alone otherwise, and splits text nodes as it likes."
  [s]
  (str/replace s #"\[(\d+)\](?![\(:])" (str "[$1](" cite-href "$1)")))

#?(:cljs
   (def ^:private katex-html
     (memoize (fn [tex display?]
                (.renderToString katex tex #js {:displayMode display? :throwOnError false})))))

(defn- formula [display?]
  (fn [_ {:keys [text]}]
    (let [tag (if display? :div.formula.display :span.formula)]
      #?(:cljs [tag {:innerHTML (katex-html text display?)}]
         :clj  [tag text]))))

(defn- renderers [{:keys [cite]}]
  (assoc md.transform/default-hiccup-renderers
         :doc (partial md.transform/into-markup [:div.rich])
         :formula (formula false)
         :block-formula (formula true)
         ;; a table may be wider than the text: it scrolls, the page does not
         :table (fn [ctx node] [:div.table-wrap (md.transform/into-markup [:table] ctx node)])
         :link (fn [ctx {:keys [attrs] :as node}]
                 (let [href (:href attrs)
                       n    (when (and href (str/starts-with? href cite-href))
                              (parse-long (subs href (count cite-href))))]
                   (cond
                     (and n cite) (cite n)
                     n            (str "[" n "]")
                     :else        (md.transform/into-markup [:a {:href href :target "_blank" :rel "noopener"}] ctx node))))))

(defn rich
  "Markdown text `s` as hiccup ([:div.rich ..]); `opts` {:cite (fn [n] hiccup)}."
  ([s] (rich s nil))
  ([s opts]
   (when-not (str/blank? s)
     (md.transform/->hiccup (renderers opts) (md/parse (mark-citations s))))))

(defn inline
  "A line of Markdown (a key point, a label) as inline hiccup: [:span.rich ..]
  when it is one paragraph, else as `rich`."
  ([s] (inline s nil))
  ([s opts]
   (let [[_ block :as h] (rich s opts)]
     (if (and (= 2 (count h)) (vector? block) (= :p (first block)))
       (into [:span.rich] (rest block))
       h))))

(defn citations
  "The numbers cited in `s`, in order of first citation."
  [s]
  (distinct (map (comp parse-long second) (re-seq #"\[(\d+)\](?![\(:])" (or s "")))))

^:rct/test
(comment
  (mark-citations "A [1]. B [2][10] [x] [link](u) end") ;=> "A [1](#cite-1). B [2](#cite-2)[10](#cite-10) [x] [link](u) end"
  (citations "A [1]. B [2][10] [1]") ;=> [1 2 10]

  (rich "Two players: $u_1(s)$, then [2].\n\n| | L | R |\n|---|---|---|\n| T | 1, 1 | 0, 0 |"
        {:cite (fn [n] [:a.cite n])})
  ;=> [:div.rich
  ;    [:p "Two players: " [:span.formula "u_1(s)"] ", then " [:a.cite 2] "."]
  ;    [:div.table-wrap [:table [:thead [:tr [:th] [:th "L"] [:th "R"]]]
  ;                      [:tbody [:tr [:td "T"] [:td "1, 1"] [:td "0, 0"]]]]]]

  (rich "$$\\max_i x_i$$") ;=> [:div.rich [:div.formula.display "\\max_i x_i"]]
  (rich "[3] without cite") ;=> [:div.rich [:p "[3]" " without cite"]]
  (rich "- **c** [1][2]" {:cite (fn [n] [:a n])}) ;=> [:div.rich [:ul [:li [[:strong "c"] " " [:a 1] [:a 2]]]]]
  (rich "  ") ;=> nil
  (inline "Payoff $u_1$ is **2**") ;=> [:span.rich "Payoff " [:span.formula "u_1"] " is " [:strong "2"]]
  )
