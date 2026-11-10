(ns robertluo.fly-learner.explore.knowledge
  "Where explorations meet the knowledge: the one namespace that knows both.

    propose!  the :common model reads passages (sections and paragraph
              groups, by their summaries) in batches and says which could
              become an exploration, and as what (a one-line pitch); the
              others are kept as :none, so each node is judged once
    make!     the :strong model writes a spec for a node from its text (the
              node and the paragraphs under it, numbered entries it cites
              [n]), as a JSON reply (a schema of the whole language is
              more than structured outputs compile); robertluo.fly-learner.explore/check
              runs it, and the problems go back for one repair. Kept with
              its anchor and cites only when sound
    anchors   a node as {:node/id :source/id :node/title}: after a
              re-ingest (new node ids) it is found again by its title in the
              same source"
  (:require [robertluo.fly-learner.prompts :as prompts]
            [clojure.string :as str]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.explore :as explore]
            [robertluo.fly-learner.explore.store :as store]
            [robertluo.fly-learner.llm :as llm]))

;;=============================================================================
;; Anchors
;;=============================================================================

(defn anchor
  "Anchor of node `node-id`, or nil when there is no such node."
  [db node-id]
  (let [{:node/keys [title source]} (d/pull db [:node/title {:node/source [:source/id]}] [:node/id node-id])]
    (when title
      {:node/id node-id :source/id (:source/id source) :node/title title})))

(defn resolve-anchor
  "The id of the node `a` points at now: its own, else the node with its
  title in the same source (re-ingested), else nil."
  [db {:node/keys [id title] sid :source/id}]
  (or (when (and id (d/entid db [:node/id id])) id)
      (when (and sid title)
        (d/q '[:find ?id . :in $ ?sid ?t
               :where [?s :source/id ?sid] [?n :node/source ?s] [?n :node/title ?t] [?n :node/id ?id]]
             db sid title))))

(def ^:private ref-pull [:node/id :node/title :node/kind :node/page {:node/source [:source/id :source/title]}])

(defn- ref-of [db a]
  (when-let [id (resolve-anchor db a)]
    (d/pull db ref-pull [:node/id id])))

;;=============================================================================
;; Material: a node and the text under it, as numbered entries
;;=============================================================================

(def ^:private max-material 24000)

(defn- descendants-in-order
  "Entity ids under `eid`, depth first in reading order."
  [db eid]
  (let [kids (->> (d/pull db [{:node/children [:db/id :node/order]}] eid)
                  :node/children (sort-by :node/order) (map :db/id))]
    (mapcat #(cons % (descendants-in-order db %)) kids)))

(defn- entry [db eid]
  (let [{:node/keys [id kind title summary text page]} (d/pull db [:node/id :node/kind :node/title :node/summary
                                                                   :node/text :node/page] eid)
        body (if (= :paragraph kind) text summary)]
    {:id id :text (str "(" (name kind) (when page (str ", p. " page)) ") "
                       (when-not (= :paragraph kind) (str title "\n"))
                       body)}))

(defn material
  "Entries for node `node-id`: itself, then what is under it in reading
  order (paragraphs with their text, groups with their summaries), up to
  `max-material` characters. [{:id node-id :text str} ..]"
  [db node-id]
  (let [eid (d/entid db [:node/id node-id])]
    (loop [[e & more] (cons eid (descendants-in-order db eid)), n 0, out []]
      (if (or (nil? e) (>= n max-material))
        out
        (let [x (entry db e)]
          (recur more (+ n (count (:text x))) (conj out x)))))))

(defn- where [db node-id]
  (let [eid (d/entid db [:node/id node-id])
        {:node/keys [title source]} (d/pull db [:node/title {:node/source [:source/title]}] eid)]
    (str/join " › " (concat [(:source/title source)] (keep :node/title (rest (db/ancestors-of db eid))) [title]))))

;;=============================================================================
;; Proposing
;;=============================================================================

(def ^:private propose-system
  (prompts/prompt!
   :explore/propose
   {:group "Exploring" :title "Pick passages to explore"
    :about "After an ingest, new sections thirty a call by their summaries (common tier): which could become an interactive exploration, and its pitch. The reply is JSON."
    :default
    "You read passages of documents a learner studies, by their summaries, and pick the
few that would be understood better by trying something out than by reading: an
interactive exploration, built later from the passage's own text.

An exploration is a small page with state the learner changes, and elements that
show the consequences: sliders, choices, toggles and buttons; tables whose cells
are computed and highlighted (payoff matrices, truth tables, comparison grids);
plots of functions of a parameter, with points; diagrams of nodes and edges
(trees, game trees, state machines, derivations, dependency graphs) whose nodes
and edges are highlighted by the state; step-throughs of a process one frame at a
time (an algorithm, a reduction, a proof's moves, a construction); and
\"predict first\" questions answered before the answer is revealed.

Propose an exploration only when doing something would teach what the passage
says: a quantity that depends on parameters, a game or matrix to analyse, an
algorithm or reduction or derivation to step through, a structure to traverse, a
rule whose outcome the learner can predict and check. The passage itself must
supply the substance (the rule, the definitions, the example, the numbers) or a
small illustrative example of it must be easy to build faithfully.

Do not propose for: overviews, introductions and conclusions, history,
motivation, related work, acknowledgements, lists of results without their
content, pure definitions with nothing to vary or step through, notation alone.
Be selective: most passages are not worth it; usually fewer than one in six.

For each one you propose: `entry` its number, `kind` the main element (table,
plot, diagram, steps, predict, slider), `pitch` one line saying what the learner
will do, concrete and specific to the passage, starting with a verb: \"Step through
the normal-order reduction of K z Ω and watch Ω vanish\", \"Move the temptation
payoff and watch which cells stay Nash equilibria\". Return an empty list when
none qualifies."}))

(def ^:private propose-schema
  {"type" "object"
   "properties" {"proposals" {"type" "array"
                              "items" {"type" "object"
                                       "properties" {"entry" {"type" "integer"}
                                                     "kind"  {"type" "string" "enum" ["table" "plot" "diagram" "steps" "predict" "slider"]}
                                                     "pitch" {"type" "string"}}
                                       "required" ["entry" "kind" "pitch"]
                                       "additionalProperties" false}}}
   "required" ["proposals"]
   "additionalProperties" false})

(defn candidates
  "Ids of the nodes worth judging (sections and paragraph groups with a
  summary) that have not been judged yet, in source order."
  [db]
  (let [judged (set (keys (store/proposals db)))]
    (->> (d/q '[:find ?id ?sid ?o :where [?n :node/kind ?k] [(contains? #{:section :group} ?k)]
                [?n :node/summary _] [?n :node/id ?id] [?n :node/order ?o]
                [?n :node/source ?s] [?s :source/id ?sid]]
              db)
         (remove (comp judged first))
         (sort-by (juxt second #(nth % 2)))
         (mapv first))))

(defn- propose-entry [db i node-id]
  (let [{:node/keys [kind summary keywords]} (d/pull db [:node/kind :node/summary :node/keywords] [:node/id node-id])]
    (str "[" (inc i) "] " (where db node-id) " (" (name kind) ")\n" summary
         (when (seq keywords) (str "\nKeywords: " (str/join ", " (sort keywords)))))))

(defn propose!
  "Judge the nodes not judged yet, `batch` at a time; forget the proposals of
  nodes that are gone. opts: :models, :batch (30), :limit (judge at most
  this many now), :on-batch (fn [{:judged :total :proposed}]). Returns
  {:judged n :proposed n}."
  [conn client {:keys [models batch limit on-batch] :or {batch 30}}]
  (let [db    (d/db conn)
        gone  (remove #(d/entid db [:node/id %]) (keys (store/proposals db)))
        _     (store/forget-proposals! conn gone)
        ids   (cond->> (candidates db) limit (take limit) true vec)
        model (llm/model-for models :common)]
    (reduce
     (fn [acc chunk]
       (let [{:keys [proposals]}
             (llm/complete-json client {:system     (prompts/text propose-system)
                                        :prompt     (str "Passages:\n\n"
                                                         (str/join "\n\n" (map-indexed #(propose-entry db %1 %2) chunk))
                                                         "\n\nWhich of these would make a good exploration?")
                                        :model      model
                                        :max-tokens 4000
                                        :schema     propose-schema})
             picked (into {} (keep (fn [{:keys [entry kind pitch]}]
                                     (when-let [id (get chunk (dec entry))]
                                       [id {:proposal/node id :proposal/kind (keyword kind) :proposal/pitch pitch}])))
                          proposals)
             acc (-> acc (update :judged + (count chunk)) (update :proposed + (count picked)))]
         (store/save-proposals! conn (mapv #(get picked % {:proposal/node % :proposal/kind :none}) chunk) model)
         (when on-batch (on-batch (assoc acc :total (count ids))))
         acc))
     {:judged 0 :proposed 0}
     (map vec (partition-all batch ids)))))

;;=============================================================================
;; Making
;;=============================================================================

(defn- blank-value? [v] (or (and (string? v) (str/blank? v)) (and (sequential? v) (empty? v))))

(defn tidy
  "A spec as the model sends it as the language has it: fields that are \"\"
  or [] dropped, inside elements and the maps in them (a model often writes
  them for none), and a plot's y-range [lo hi] as y-min, y-max."
  [spec]
  (letfn [(drop-blank [m] (into {} (remove (comp blank-value? val)) m))
          (clean [x] (cond (map? x) (update-vals (drop-blank x) clean)
                           (sequential? x) (mapv clean x)
                           :else x))]
    (update spec :view
            (fn [els]
              (mapv (fn [el]
                      (let [el (clean el)
                            [lo hi] (:y-range el)]
                        (cond-> (dissoc el :y-range)
                          (and (number? lo) (number? hi) (< lo hi)) (assoc :y-min lo :y-max hi))))
                    els)))))

(def ^:private make-system
  (prompts/prompt!
   :explore/make
   {:group "Exploring" :title "Build an exploration"
    :about "Explore it (strong tier): writes the exploration's spec in the language this prompt describes; the app checks it by running it. Change the language and the specs stop working."
    :default
    "You build explorations: small interactive pages that let a learner understand a
passage of a document they study by trying it out. You write the page as a spec,
data in the language below; the app draws it and runs it.

# What makes a good exploration

- It teaches what the passage says. The learner changes something (a parameter, a
  choice, a step) and sees the consequence the passage describes. Prefer one clear
  idea, well shown, over many.
- It is faithful. Rules, definitions, numbers and steps come from the entries, and
  the text cites them: [n] cites entry n, right after the claim, one number per
  bracket ([2][5]). When you need a concrete example the entries lack (numbers for
  a payoff matrix, a small term to reduce), make it small and correct and say in
  the intro that it is an illustrative example. Never state as the document's what
  it does not say.
- Computations are computed: when the page shows a value that follows from the
  state (a best response, a sum, whether a condition holds), derive it with an
  expression instead of writing it out, so it stays right as the learner moves
  things. Precomputed sequences (the frames of a reduction or proof) must be
  correct; check each step.
- It is small: usually 3 to 8 elements. A \"predict\" first, when there is a
  non-obvious outcome to guess, makes the learner think before seeing.
- Plain, short text, in the document's notation. The title says what the learner
  will see or do, at most 10 words.

# The spec

    title   string
    intro   template: what this is, one to three sentences
    vars    [{name, init}]: the state the learner changes; init is an expression
            (it may use earlier vars)
    derive  [{name, expr}]: values computed from the state, in order (each may use
            the vars and the derived values before it)
    view    [element]: shown top to bottom

Names are plain words (a-z, digits, - ? *), not the builtins below.

## Templates and expressions

A template is text; an expression in braces is replaced by its value: \"T = {t}\",
\"{(if (> t r) \\\"defect\\\" \\\"cooperate\\\")}\". A literal { is written {{ (sets:
\"{{0, 1}\"); } alone is literal.

An expression is a string holding one s-expression, Clojure-like:
- values: numbers, strings in double quotes (\"Defect\"), true false nil, vectors
  [1 2], maps {\"a\" 1}
- names: vars, derived values, and the locals of an element (below)
- (if c a b) (when c a) (cond c1 a c2 b :else d) (let [x 1 [a b] pair] body)
  (fn [x] body) (and ..) (or ..)
- builtins: + - * / mod quot rem inc dec abs min max round floor ceil sqrt pow exp
  log = not= < > <= >= not nil? some? number? string? zero? pos? neg? even? odd?
  count nth get get-in first second last rest take drop conj assoc assoc-in dissoc
  update keys vals concat vec vector list hash-map set range repeat map
  map-indexed filter remove reduce some every? apply sort sort-by reverse distinct
  contains? index-of argmax (index of the largest) column (j-th column of a
  matrix) sum str subs upper-case lower-case includes? join (fmt x digits)
- nothing else: no Java, no namespaces, no def, no loops but map/filter/reduce/range;
  = compares numbers by value
- a value in a template reads: numbers to at most 3 decimals, vectors as (a, b),
  nil as nothing. Use (fmt x 2) for 2 decimals.

## Elements

Every element has `type`, and may have `show`: an expression; the element is
shown only when it is true. Leave out the fields an element does not use.

    text     text: template
    formula  text: template, shown monospace (terms, formulas); mark: template, a
             substring of text to highlight (e.g. the redex)
    slider   var, min, max, step (numbers), label: template (show the value in it:
             \"Temptation T: {t}\")
    choice   var, label, options: [{label, value}]; value is an expression, the
             var gets it
    toggle   var, label; the var is true or false
    button   label, set: [{var, expr}] applied in order
    table    rows, cols: [template]; either cell: one expression computing the cell
             from row and col (0-based indices), or cells: [[template]] rows of
             literal cells; highlight: expression (row, col and value bound), the
             cell is highlighted when true; on-click: [{var, expr}] (row, col,
             value bound); caption: template
    plot     x-min, x-max (numbers), y-min, y-max (numbers, leave out to fit),
             x-label, y-label; series: [{y, label}], y an expression of x drawn over the
             range; points: [{x, y, label}] expressions, e.g. the current state
    diagram  nodes: [{id, label (template, \\n for a new line), highlight, show}],
             edges: [{from, to, label, highlight}] (from, to are node ids);
             highlight and show are expressions with id (nodes) or from, to (edges)
             bound; on-click: [{var, expr}] with id bound; direction: down (trees,
             the default) or right (sequences). Laid out by rank from the nodes with
             no incoming edge
    steps    var (the step index, starts at 0), frames: [{caption: template,
             set: [{var, expr}]}]. Showing frame i applies the sets of frames 0..i in
             order to the state (each set sees the values derived so far), so frames
             describe what changes. Other elements show the state, e.g. a formula
             showing a term var the frames set. Back/next buttons are drawn
    predict  var, prompt: template, options: [{label}], answer: expression giving the
             index (0-based) of the right option, explain: template shown with the
             answer once the learner has picked

An input's var need not be declared in vars when its start is obvious (slider:
min, toggle: false, choice: the first option, steps: 0, predict: none); declare it
to start elsewhere.

# Example

{\"title\": \"Prisoner's dilemma: where is the equilibrium?\",
 \"intro\": \"A cell holds (row's payoff, column's payoff) [1]; highlighted cells are Nash equilibria. The payoffs are an illustrative example.\",
 \"vars\": [{\"name\": \"t\", \"init\": \"5\"}, {\"name\": \"r\", \"init\": \"3\"}, {\"name\": \"p\", \"init\": \"1\"}, {\"name\": \"s\", \"init\": \"0\"}],
 \"derive\": [{\"name\": \"A\", \"expr\": \"[[r s] [t p]]\"}, {\"name\": \"B\", \"expr\": \"[[r t] [s p]]\"}],
 \"view\": [
  {\"type\": \"predict\", \"var\": \"guess\", \"prompt\": \"Which outcome is an equilibrium?\", \"options\": [{\"label\": \"Both cooperate\"}, {\"label\": \"Both defect\"}], \"answer\": \"1\", \"explain\": \"Defecting pays more whatever the other does [2].\"},
  {\"type\": \"slider\", \"var\": \"t\", \"min\": 0, \"max\": 8, \"step\": 1, \"label\": \"Temptation T: {t}\"},
  {\"type\": \"table\", \"rows\": [\"Cooperate\", \"Defect\"], \"cols\": [\"Cooperate\", \"Defect\"],
   \"cell\": \"[(get-in A [row col]) (get-in B [row col])]\",
   \"highlight\": \"(and (= (get-in A [row col]) (apply max (column A col))) (= (get-in B [row col]) (apply max (nth B row))))\",},
  {\"type\": \"text\", \"text\": \"{(if (> t r) \\\"T > R: both defect, though both would rather cooperate.\\\" \\\"T ≤ R: mutual cooperation is an equilibrium too.\\\")}\"}]}

# Reply

The spec as one JSON object, and nothing else: no prose, no code fence.

# Checking

Your spec is run before it is kept: every expression read, the page shown from its
start and from each state one action away (each frame, each slider at both ends,
each option, each click). If something fails you get the problems back; return the
whole spec corrected."}))

(defn- entries-text [entries]
  (str/join "\n\n" (map-indexed (fn [i {:keys [text]}] (str "[" (inc i) "] " text)) entries)))

(defn- make-prompt [db node-id entries {:keys [pitch feedback previous]}]
  (str "Passage: " (where db node-id) "\n\n"
       "Entries:\n\n" (entries-text entries) "\n\n"
       "Make an exploration of this passage"
       (if (str/blank? pitch) "." (str ": " pitch))
       (when previous
         (str "\n\nAn earlier version:\n" (json/write-value-as-string previous)))
       (when-not (str/blank? feedback)
         (str "\n\nThe learner asks for this version: " feedback))))

(defn- spent [before] (- (:total (llm/cost) 0) (:total before 0)))

(defn read-reply
  "The JSON object in a model's reply, keyword keys: the first { from which
  the text up to the last } parses (prose or a code fence around it, braces
  in that prose, are passed over); or {::unreadable message}."
  [text]
  (let [j (str/last-index-of text "}")]
    (loop [i (str/index-of text "{"), err "no JSON object in the reply"]
      (if (or (nil? i) (nil? j) (>= i j))
        {::unreadable err}
        (let [r (try (let [v (json/read-value (subs text i (inc j)) json/keyword-keys-object-mapper)]
                       (when (map? v) v))
                     (catch Exception e (str "not valid JSON: " (ex-message e))))]
          (if (map? r)
            r
            (recur (str/index-of text "{" (inc i)) (if (string? r) r err))))))))

^:rct/test
(comment
  (read-reply "Here:\n```json\n{\"title\": \"T\", \"view\": []}\n```") ;=> {:title "T" :view []}
  (contains? (read-reply "{\"title\": ") ::unreadable) ;=> true
  ;; braces in prose before the object
  (:title (read-reply "Over {0, 1}: here it is {\"title\": \"T\"}")) ;=> "T"
  (tidy {:view [{:type "text" :text "a" :show "" :cells []}
                {:type "plot" :y-range [0 5] :points [{:x "1" :y "2" :label ""}]}]})
  ;=> {:view [{:type "text" :text "a"} {:type "plot" :y-min 0 :y-max 5 :points [{:x "1" :y "2"}]}]}
  )

(defn- write-spec
  "[reply-text spec-or-unreadable]"
  [client model prompt]
  (let [text (llm/complete client {:system (prompts/text make-system) :prompt prompt :model model :max-tokens 16000})]
    [text (read-reply text)]))

(defn make!
  "Write, check and keep an exploration of node `node-id`. opts: :models,
  :pitch (what to make; default: the node's proposal), :feedback and
  :previous (an exploration id) to remake one. Returns {:exploration/id ..}
  or {:error message :problems [..]} when no sound spec came."
  [conn client node-id {:keys [models pitch feedback previous]}]
  (let [db      (d/db conn)
        before  (llm/cost)
        model   (llm/model-for models :strong)
        prev    (when previous (store/by-id db previous))
        pitch   (or pitch (:exploration/pitch prev) (:proposal/pitch (store/proposal db node-id)))
        entries (material db node-id)
        prompt  (make-prompt db node-id entries {:pitch pitch :feedback feedback
                                                 :previous (some-> prev :exploration/spec (dissoc :cites))})
        spec-of   #(assoc (explore/normalize (tidy %)) :cites (mapv :id entries))
        problems-of (fn [raw] (if-let [u (::unreadable raw)] [u] (explore/check (spec-of raw))))
        [text1 raw1] (write-spec client model prompt)
        problems  (problems-of raw1)
        [raw problems] (if (empty? problems)
                         [raw1 problems]
                         (let [_ (println (str "explore: repairing " node-id ": " (str/join "; " (take 5 problems))))
                               [_ again] (write-spec client model
                                                     (str prompt "\n\nYour spec:\n" text1
                                                          "\n\nProblems found when it was checked and run:\n"
                                                          (str/join "\n" (map #(str "- " %) problems))
                                                          "\n\nReply with the whole spec, corrected."))]
                           [again (problems-of again)]))]
    (if (seq problems)
      {:error (str "The exploration did not work out: " (first problems)
                   (when (next problems) (str " (and " (dec (count problems)) " more)")) ". Try again?")
       :problems problems}
      {:exploration/id
       (store/save! conn (cond-> {:exploration/title  (:title raw)
                                  :exploration/spec   (spec-of raw)
                                  :exploration/anchor (anchor db node-id)
                                  :exploration/cites  (mapv #(anchor db (:id %)) entries)
                                  :exploration/model  model
                                  :exploration/cost   (double (spent before))}
                           pitch    (assoc :exploration/pitch pitch)
                           (not (str/blank? feedback)) (assoc :exploration/feedback feedback)
                           previous (assoc :exploration/previous previous)))})))

;;=============================================================================
;; Reading back
;;=============================================================================

(defn exploration
  "Exploration `id` for display: its spec, the node it is about and the
  nodes its [n] cite (nil for ones gone), as node refs."
  [db id]
  (when-let [x (store/by-id db id)]
    (-> x
        (dissoc :exploration/anchor :exploration/model)
        (assoc :exploration/node (ref-of db (:exploration/anchor x))
               :exploration/cites (mapv #(ref-of db %) (:exploration/cites x))))))

(defn all
  "Every exploration, newest first, with the node it is about."
  [db]
  (mapv (fn [x] (-> x (dissoc :exploration/anchor) (assoc :exploration/node (ref-of db (:exploration/anchor x)))))
        (store/all db)))

(defn for-node
  "What node `node-id` offers: {:proposal {:kind :pitch} (nil when none)
  :explorations [{:exploration/id :exploration/title}]}."
  [db node-id]
  (let [p (store/proposal db node-id)]
    {:proposal     (when (and p (not= :none (:proposal/kind p)))
                     {:kind (:proposal/kind p) :pitch (:proposal/pitch p)})
     :explorations (->> (store/all db)
                        (filter #(= node-id (resolve-anchor db (:exploration/anchor %))))
                        (mapv #(select-keys % [:exploration/id :exploration/title :exploration/at])))}))
