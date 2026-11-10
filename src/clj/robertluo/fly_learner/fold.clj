(ns robertluo.fly-learner.fold
  "Fold a section tree (see robertluo.fly-learner.ingest.outline) into a layered knowledge DAG.

  1. `regroup`   long runs of paragraphs inside a section are split into
                 groups, so no node has too many children to scan.
  2. `summarize-tree`  bottom-up: every section/group gets a title, summary and
                 keywords from its children (sibling calls run in parallel).
  3. `flatten-tree`    nodes with ids, levels (0 = birdview) and child edges.
  4. `link-by-keywords` cross links between nodes sharing specific keywords;
                 these make the structure a DAG rather than a tree.

  Summarisation is a function argument so folding can be tested without a
  model. See `llm-summarizer` for the real one.
  Reference: https://github.com/chenxiachan/textfold"
  (:require [clojure.string :as str]
            [robertluo.fly-learner.llm :as llm])
  (:import (java.util.concurrent Semaphore)))

(defn- internal? [x] (contains? x :children))

;; --- 1. regroup -----------------------------------------------------------

(def default-opts {:max-children 10 :group-size 6})

(defn regroup
  "Split runs of more than `group-size` consecutive paragraphs into groups,
  in sections that have more than `max-children` children."
  ([tree] (regroup tree default-opts))
  ([tree {:keys [max-children group-size] :as opts}]
   (let [children (mapv #(if (internal? %) (regroup % opts) %) (:children tree))]
     (assoc tree :children
            (if (<= (count children) max-children)
              children
              (into []
                    (mapcat (fn [run]
                              (if (or (internal? (first run)) (<= (count run) group-size))
                                run
                                (map (fn [g] {:group true :page (:page (first g)) :children (vec g)})
                                     (partition-all group-size run)))))
                    (partition-by internal? children)))))))

;; --- 2. summarize ---------------------------------------------------------

(defn summarize-tree
  "Bottom-up summarisation. `summarize` receives
    {:path [ancestor titles] :heading title-or-nil
     :kind :source|:section|:group :items [child ...]}
  and returns {:title .. :summary .. :keywords [..]}.
  The heading of a real section is kept as its title; a section's :tags
  (the author's, from a Markdown chapter's front matter) join its keywords."
  ([summarize tree] (summarize-tree summarize tree nil))
  ([summarize tree path]
   (let [root?    (nil? path)
         heading  (when-not (or root? (:group tree)) (:title tree))
         here     (cond root? [(:title tree)] heading (conj path heading) :else path)
         children (->> (:children tree)
                       (mapv #(if (internal? %) (future (summarize-tree summarize % here)) %))
                       (mapv #(if (future? %) @% %)))
         kind     (cond root? :source (:group tree) :group :else :section)
         result   (summarize {:path here :heading heading :kind kind :items children})]
     (-> tree
         (assoc :children children
                :summary  (:summary result)
                :keywords (set (map str/lower-case (concat (:keywords result) (:tags tree)))))
         (assoc :title (or heading (:title result)))))))

;; --- 3. flatten -----------------------------------------------------------

(defn- leaf-title [text]
  (let [t (str/replace (str/trim text) #"\s+" " ")]
    (if (> (count t) 60) (str (subs t 0 57) "...") t)))

(defn flatten-tree
  "Depth-first list of nodes; :node/level is the depth (0 = root)."
  [tree]
  (let [out (volatile! [])]
    (letfn [(walk [x level order]
              (let [id   (random-uuid)
                    kids (when (internal? x)
                           (doall (map-indexed (fn [i c] (walk c (inc level) i)) (:children x))))]
                (vswap! out conj (cond-> {:node/id    id
                                    :node/level level
                                    :node/order order
                                    :node/kind  (cond (not (internal? x)) :paragraph
                                                      (:group x) :group
                                                      (zero? level) :source
                                                      :else :section)
                                    :node/title (if (internal? x) (:title x) (leaf-title (:text x)))}
                             (:page x)          (assoc :node/page (:page x))
                             (:text x)          (assoc :node/text (:text x))
                             (:summary x)       (assoc :node/summary (:summary x))
                             (seq (:keywords x)) (assoc :node/keywords (:keywords x))
                             kids               (assoc :node/children (vec kids))))
                id))]
      (walk tree 0 0))
    @out))

;; --- 4. links -------------------------------------------------------------

(defn link-by-keywords
  "Link summarised nodes that share a keyword used by at most `max-share`
  nodes (very common keywords would link everything). Direct parent/child
  pairs are not linked; they are already connected."
  ([nodes] (link-by-keywords nodes {:max-share 6}))
  ([nodes {:keys [max-share]}]
   (let [edges   (set (for [n nodes, c (:node/children n)] #{(:node/id n) c}))
         by-kw   (reduce (fn [m n] (reduce #(update %1 %2 (fnil conj []) (:node/id n)) m (:node/keywords n)))
                         {} nodes)
         links   (reduce (fn [m ids]
                           (reduce (fn [m [a b]] (-> m (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))
                                   m
                                   (for [a ids, b ids :when (and (not= a b) (not (edges #{a b})))] [a b])))
                         {}
                         (filter #(<= 2 (count %) max-share) (vals by-kw)))]
     (mapv #(if-let [ls (links (:node/id %))] (assoc % :node/links (vec ls)) %) nodes))))

;; --- persistence ----------------------------------------------------------

(defn ->tx
  "Transaction data for a source and its folded `nodes`: the source's `id`
  (a new random one when nil), `title`, `path`, content `hash`, `type`,
  `skip` (a Markdown folder's parts left out).
  Node uuids double as tempids so children/links resolve in one transaction."
  [{:keys [id title path hash type skip] :or {type :pdf}} nodes]
  (let [tid    str
        root   (first (filter #(zero? (:node/level %)) nodes))
        source (cond-> {:db/id "source" :source/id (or id (random-uuid)) :source/type type
                        :source/title (or (:node/title root) title) :source/path path}
                 hash        (assoc :source/hash hash)
                 (seq skip)  (assoc :source/skip (vec skip)))]
    (into [(cond-> source root (assoc :source/root (tid (:node/id root))))]
          (map (fn [n]
                 (cond-> (assoc n :db/id (tid (:node/id n)) :node/source "source")
                   (:node/children n) (update :node/children #(mapv tid %))
                   (:node/links n)    (update :node/links #(mapv tid %)))))
          nodes)))

;; --- LLM summariser -------------------------------------------------------

(def ^:private system-prompt
  "You are building a layered study map of a document, so a learner can
understand it top-down and drill into detail only where needed.

You receive one part of the document: either original passages, or the
titles and summaries of its sub-parts. Produce:
- title: specific, at most 8 words (it is replaced by the heading when the
  part has one).
- summary: what this part establishes and how its pieces connect, in plain
  language. 2-4 sentences for a few passages, up to 8 for a whole chapter or
  document. Briefly gloss notation only when the idea depends on it. Do not
  open with \"This section\".
- keywords: 3-8 key concepts, lowercase, in their canonical name (e.g.
  \"strong normalization\", \"calculus of constructions\"), not generic words
  like \"proof\" or \"definition\". Prefer the same wording for the same
  concept across parts, so related parts can be linked.

The document's outline is below for orientation; the part you summarise is
named in the request.")

(defn outline-text
  "Indented heading list of a section tree, used as shared context."
  [tree]
  (letfn [(walk [t depth]
            (when (and (:children t) (not (:group t)))
              (cons (str (apply str (repeat depth "  ")) (:title t))
                    (mapcat #(walk % (inc depth)) (:children t)))))]
    (str/join "\n" (walk tree 0))))

(def ^:private summary-schema
  {:type "object"
   :properties {:title {:type "string"}
                :summary {:type "string"}
                :keywords {:type "array" :items {:type "string"}}}
   :required ["title" "summary" "keywords"]
   :additionalProperties false})

(defn- render-item [i x]
  (if (internal? x)
    (str "[" (inc i) "] " (:title x) "\n" (:summary x))
    (str "[" (inc i) "] " (:text x))))

(defn tier-for
  "Runs of paragraphs are routine (:common); sections, chapters and the
  whole document need synthesis (:strong)."
  [kind]
  (if (= :group kind) :common :strong))

(defn llm-summarizer
  "Summariser backed by Claude.
  opts: :outline  outline text of the document (shared, cached prefix)
        :models   tier -> model id overrides (see robertluo.fly-learner.llm/default-models)
        :concurrency  max parallel requests (default 8)
        :on-call  invoked after each request"
  [client {:keys [outline models concurrency on-call] :or {concurrency 8 on-call (fn [])}}]
  (let [permits (Semaphore. concurrency)
        system  (cond-> system-prompt outline (str "\n\n<outline>\n" outline "\n</outline>"))]
    (fn [{:keys [path heading kind items]}]
      (let [prompt (str "Part: " (str/join " > " path) "\n"
                        (when heading (str "Heading: " heading "\n"))
                        "\n" (str/join "\n\n" (map-indexed render-item items)))
            req    {:system system :prompt prompt :schema summary-schema :effort :medium}
            tier   (tier-for kind)]
        (.acquire permits)
        (try
          (try
            (llm/complete-json client (assoc req :model (llm/model-for models tier)))
            (catch clojure.lang.ExceptionInfo e
              ;; the :common tier has no server-side refusal fallback
              (if (and (llm/refusal? e) (= :common tier))
                (llm/complete-json client (assoc req :model (llm/model-for models :strong)))
                (throw e))))
          (finally
            (.release permits)
            (on-call)))))))

;; --- estimate -------------------------------------------------------------

(def ^:private reply-tokens
  "About what a summary call writes (title, summary, keywords and its
  reasoning), by tier: so that the 116-page thesis first folded (README:
  $0.23) is estimated at $0.26."
  {:strong 500 :common 250})

(defn estimate
  "About what folding `tree` (regrouped) with `llm-summarizer` costs, before
  any call: {:calls {tier n} :usd x}. Tokens taken as characters / 4; a
  sub-part's title and summary as 700 characters; the system prompt and
  outline written to the cache once per tier, then read from it."
  ([tree] (estimate tree nil))
  ([tree models]
   (let [system (quot (+ (count system-prompt) (count (outline-text tree))) 4)
         calls  (letfn [(walk [t root?]
                          (when (internal? t)
                            (cons {:tier  (tier-for (cond root? :source (:group t) :group :else :section))
                                   :input (quot (+ 300 (reduce + (map #(if (internal? %) 700 (count (:text %)))
                                                                    (:children t))))
                                                4)}
                                  (mapcat #(walk % false) (:children t)))))]
                  (walk tree true))
         by     (group-by :tier calls)]
     {:calls (update-vals by count)
      :usd   (reduce + (for [[tier cs] by
                             :let [[in out] (llm/prices (llm/model-for models tier) [0 0])
                                   n        (count cs)]]
                         (/ (+ (* in (+ (reduce + (map :input cs))
                                        (* 1.25 system)
                                        (* 0.1 system (dec n))))
                               (* out n (reply-tokens tier)))
                            1e6)))})))

^:rct/test
(comment
  (:calls (estimate {:title "Book" :children [{:title "A" :children [{:text "one"} {:text "two"}]}
                                              {:group true :children [{:text "three"}]}]}))
  ;=> {:strong 2 :common 1}
  )
