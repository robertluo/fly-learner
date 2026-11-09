(ns robertluo.fly-learner.concept
  "Concepts: the ideas a knowledge base is about, each with an identity.

  A node is a passage of one source. A concept is an idea that many passages,
  in any source, talk about. Folding tags nodes with keywords, which are plain
  strings: one idea gets several spellings (\"equality type\", \"equality
  types\"), and one spelling can name related but different ideas (\"untyped
  equality\" is a kind of equality type, not the same idea). A concept gives an
  idea one identity:

    :concept/label    display name
    :concept/aliases  the keywords naming it (label included). Unique in the
                      schema: a keyword names at most one concept.
    :concept/gloss    one sentence fixing its sense. Identity hinges on the
                      sense, not the spelling.
    :concept/broader  is-a / part-of parents
    :concept/by       who last asserted it: a model id or \"human\"

  Mentions are derived, not stored: node n mentions concept c when one of n's
  keywords is an alias of c. Moving an alias therefore moves its mentions.

  Who writes concepts:
    `resolve!`  a model assigns the keywords no concept covers yet. It only
                adds: existing concepts gain aliases and broader links, and
                their label and gloss stay as they are.
    curation    `rename!` `set-gloss!` `merge!` `split!` `assign!` `ignore!`
                `unignore!` `add-broader!` `remove-broader!` `retag!` (a
                homonym: one keyword, different ideas in different nodes),
                `add!` (a concept the model missed, tagged on the passages
                naming it: `naming-nodes`), `remove!` (not a concept: its
                keywords ignored from then on): human corrections.
                Each is one atomic change.
  Either writer can be wrong, so every change is logged as an :edit/* entity
  (op, args, by, at). That is the audit trail, and the args are enough to
  replay the edit after a rebuild.

  The concept graph and its operations: robertluo.fly-learner.concept.graph."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.llm :as llm]))

;;=============================================================================
;; Reading
;;=============================================================================

(defn concepts
  "All concepts, aliases as a set and broader as a set of :concept/id."
  [db]
  (->> (d/q '[:find [(pull ?c [:db/id :concept/id :concept/label :concept/gloss :concept/aliases
                               :concept/by :concept/origin :concept/prereqs-by
                               {:concept/broader [:concept/id]}]) ...]
              :where [?c :concept/id]]
            db)
       (mapv #(-> %
                  (update :concept/aliases set)
                  (update :concept/broader (fn [bs] (set (map :concept/id bs))))))))

(defn keyword-usage
  "keyword -> {:count nodes-using-it :titles [up to 3 titles, highest level first]}."
  [db]
  (let [titles (into {} (d/q '[:find ?n ?t :where [?n :node/keywords _] [?n :node/title ?t]] db))
        levels (into {} (d/q '[:find ?n ?l :where [?n :node/keywords _] [?n :node/level ?l]] db))]
    (->> (d/q '[:find ?k ?n :where [?n :node/keywords ?k]] db)
         (reduce (fn [m [k n]] (update m k (fnil conj []) n)) {})
         (into {} (map (fn [[k ns]]
                         [k {:count  (count ns)
                             :titles (->> ns (sort-by levels) (map titles) distinct (take 3) vec)}]))))))

(defn unresolved
  "Keywords no concept names and none ignored, with their usage (sorted map)."
  [db]
  (let [taken (into (set (d/q '[:find [?a ...] :where [_ :concept/aliases ?a]] db))
                    (d/q '[:find [?k ...] :where [_ :ignored/keyword ?k]] db))]
    (into (sorted-map) (remove (comp taken key)) (keyword-usage db))))

(defn- head-word
  "Last word with a plural -s dropped: a sort key that puts \"equality
  type\", \"equality types\" and \"dependent function type\" near each other."
  [kw]
  (-> (str/split kw #"\s+") last (str/replace #"(?<=\p{L}{3})s$" "")))

(defn batches
  "`keywords` in batches of `size`, variants of one head word kept together
  where possible."
  [keywords size]
  (partition-all size (sort-by (juxt head-word identity) keywords)))

^:rct/test
(comment
  (batches ["intersection types" "equality type" "untyped equality" "equality types"
            "dependent intersection" "intersection type"] 3)
  ;=> [["untyped equality" "dependent intersection" "equality type"]
  ;    ["equality types" "intersection type" "intersection types"]]
  )

;;=============================================================================
;; Edits
;;=============================================================================

(defn- edit [op by args]
  {:edit/op op :edit/by by :edit/args args :edit/at (java.util.Date.)})

(defn- apply-stages!
  "Transact `stages` (tx-data vectors) atomically, the edit log entry with
  the last. Stages exist because Datalevin checks a unique value against
  the state before the transaction: an alias must leave one concept in a
  transaction before it can join another."
  [conn stages log]
  (d/with-transaction [cn conn]
    (doseq [tx (butlast stages)] (d/transact! cn (vec tx)))
    (d/transact! cn (conj (vec (last stages)) log)))
  nil)

(def ^:private human "human")

(defn- eid! [db id]
  (or (d/entid db [:concept/id id]) (throw (ex-info "No such concept" {:concept/id id}))))

(defn- norm [s] (-> s str/trim str/lower-case))

(defn- owner
  "eid of the concept with alias `alias`, or nil."
  [db alias]
  (d/q '[:find ?c . :in $ ?a :where [?c :concept/aliases ?a]] db alias))

;;=============================================================================
;; Resolution (model-written)
;;=============================================================================

(defn resolution->tx
  "Tx data for a resolver's `reply` about `keywords`, given the `existing`
  concepts (see `concepts`).

  A reply concept whose label names an existing concept (its label or an
  alias) adds its keywords and broader links to that concept. Any other
  reply concept becomes a new concept. Keywords not in `keywords`, or
  already given to an earlier reply concept, are dropped. Broader labels
  that name no concept are dropped. A new concept with no keywords is kept
  only when another concept names it as broader.

  Returns {:tx [...] :new n :attached n :ignored n :missing #{keyword ...}}."
  [existing keywords {:keys [concepts ignored]} by]
  (let [wanted   (set keywords)
        by-name  (into {} (for [c existing, a (conj (:concept/aliases c) (:concept/label c))]
                            [(norm a) (:db/id c)]))
        ;; reply concepts with one label are one concept
        merged   (->> concepts
                      (keep (fn [c] (let [l (norm (or (:label c) ""))] (when-not (str/blank? l) (assoc c :label l)))))
                      (reduce (fn [[order m] {:keys [label] :as c}]
                                (if-let [prev (m label)]
                                  [order (assoc m label (-> prev
                                                            (update :keywords concat (:keywords c))
                                                            (update :broader concat (:broader c))
                                                            (update :gloss #(if (str/blank? %) (:gloss c) %))))]
                                  [(conj order label) (assoc m label c)]))
                              [[] {}])
                      ;; reply order decides who keeps a contested keyword
                      ((fn [[order m]] (map m order))))
        ;; each wanted keyword goes to the first reply concept listing it
        [assigned taken]
        (reduce (fn [[out taken] {:keys [keywords] :as c}]
                  (let [ks (->> keywords (map norm) (filter wanted) (remove taken) distinct vec)]
                    [(conj out (assoc c :keywords ks)) (into taken ks)]))
                [[] #{}] merged)
        target   (into {} (for [{:keys [label]} assigned]
                            [label (or (by-name label) (str "concept:" label))]))
        known    (fn [l] (or (by-name (norm l)) (target (norm l))))
        broader  (into {} (for [{:keys [label broader]} assigned]
                            [label (->> broader (keep known) (remove #{(target label)}) distinct vec)]))
        parents  (set (mapcat val broader))
        fresh    (filter (fn [{:keys [label keywords]}]
                           (and (string? (target label))
                                (or (seq keywords) (parents (target label)))))
                         assigned)
        attached (filter #(and (not (string? (target (:label %)))) (seq (:keywords %))) assigned)
        kept     (set (map (comp target :label) fresh))
        live?    #(or (not (string? %)) (kept %))
        ignore   (->> ignored (map norm) (filter wanted) (remove taken) distinct)]
    {:tx       (vec (concat
                     (for [{:keys [label gloss keywords]} fresh]
                       (cond-> {:db/id           (target label)
                                :concept/id      (random-uuid)
                                :concept/label   label
                                ;; the label names it too, unless a keyword spelt so went elsewhere
                                :concept/aliases (vec (distinct (cond-> keywords
                                                                  (not (taken label)) (conj label))))
                                :concept/by      by}
                         (not (str/blank? gloss)) (assoc :concept/gloss gloss)))
                     (for [{:keys [label keywords]} attached, k keywords]
                       [:db/add (target label) :concept/aliases k])
                     (for [[label ps] broader
                           :when (live? (target label))
                           p ps :when (live? p)]
                       [:db/add (target label) :concept/broader p])
                     (for [k ignore] {:ignored/keyword k})))
     :new      (count fresh)
     :attached (count attached)
     :ignored  (count ignore)
     :missing  (into (sorted-set) (remove (-> taken (into ignore) (into (map :label fresh)))) wanted)}))

(defn resolve!
  "Give every unresolved keyword (see `unresolved`) a concept, or ignore it,
  in batches of `:batch-size` (default 80). Each batch sees the concepts the
  earlier ones made.

  `resolver` gets {:concepts (concepts db) :keywords [{:keyword :count
  :titles} ...]} and returns {:concepts [{:label :gloss :keywords [..]
  :broader [..]} ..] :ignored [..] :by who}; see `llm-resolver`. Keywords it
  leaves out stay unresolved for the next run. Each batch is logged as a
  :resolve edit with the reply. `:on-batch` is called with each batch's
  result.

  Returns totals {:batches :new :attached :ignored :missing}."
  ([conn resolver] (resolve! conn resolver {}))
  ([conn resolver {:keys [batch-size on-batch] :or {batch-size 80 on-batch (fn [_])}}]
   (let [todo (unresolved (d/db conn))]
     (reduce (fn [acc batch]
               (let [existing (concepts (d/db conn))
                     reply    (resolver {:concepts existing
                                         :keywords (mapv #(assoc (todo %) :keyword %) batch)})
                     by       (:by reply "unknown")
                     {:keys [tx] :as r} (resolution->tx existing batch reply by)]
                 (d/transact! conn (conj tx (edit :resolve by {:keywords (vec batch)
                                                               :reply (dissoc reply :by)})))
                 (on-batch (dissoc r :tx))
                 (-> (merge-with + acc (select-keys r [:new :attached :ignored]))
                     (update :batches inc)
                     (update :missing into (:missing r)))))
             {:batches 0 :new 0 :attached 0 :ignored 0 :missing (sorted-set)}
             (batches (keys todo) batch-size)))))

;;-----------------------------------------------------------------------------
;; the model

(def ^:private system-prompt
  "You maintain the concept index of a personal knowledge base. Documents are
folded into summarised parts, and each part is tagged with keywords. Keywords
are plain strings, so one idea appears under several spellings (\"equality
type\", \"equality types\"), and one spelling may name different ideas in
different fields. A concept gives an idea one identity, and the knowledge
graph is built over concepts.

You receive the concepts that exist so far and a batch of new keywords. Each
keyword comes with how many parts use it and the titles of some of them.
Assign every new keyword to exactly one concept, or ignore it.

- Same idea, different wording (plural, word order, abbreviation, a
  notation's name vs. its spelled-out name): one concept.
- Different ideas get different concepts even when their words overlap.
  \"untyped equality\" and \"definitional equality\" are kinds of equality,
  not the same concept: give each its own concept and relate them with
  broader. Do not merge a specific idea into a general one because they
  share a word.
- To add keywords to an existing concept, give its label exactly, an empty
  gloss and only the new keywords. Its gloss is kept.
- A new concept: label is its canonical name, lowercase, singular; gloss is
  one sentence saying what it is in this field, specific enough to tell it
  apart from other senses of the same words.
- broader: labels of the concepts (existing or in your reply) that this one
  is a kind of or a part of. Direct parents only, usually zero to two. You
  may create a concept without keywords only to serve as a broader parent of
  others.
- ignored: keywords too generic to name a specific idea, e.g. \"proof\",
  \"definition\", \"example\", \"motivation\".")

(def ^:private reply-schema
  {:type "object"
   :properties {:concepts {:type  "array"
                           :items {:type "object"
                                   :properties {:label    {:type "string"}
                                                :gloss    {:type "string"}
                                                :keywords {:type "array" :items {:type "string"}}
                                                :broader  {:type "array" :items {:type "string"}}}
                                   :required ["label" "gloss" "keywords" "broader"]
                                   :additionalProperties false}}
                :ignored  {:type "array" :items {:type "string"}}}
   :required ["concepts" "ignored"]
   :additionalProperties false})

(defn- concept-line [{:concept/keys [label gloss aliases]}]
  (let [others (sort (disj aliases label))]
    (str "- " label (when gloss (str ": " gloss))
         (when (seq others) (str " (also: " (str/join ", " (take 6 others)) ")")))))

(defn- keyword-line [{:keys [keyword count titles]}]
  (str "- \"" keyword "\" (" count "; " (str/join " | " titles) ")"))

(defn resolver-prompt
  "The user prompt for one batch."
  [{:keys [concepts keywords]}]
  (str "Existing concepts:\n"
       (if (seq concepts)
         (str/join "\n" (map concept-line (sort-by :concept/label concepts)))
         "(none yet)")
       "\n\nNew keywords (parts using it; titles of some):\n"
       (str/join "\n" (map keyword-line keywords))))

(defn llm-resolver
  "Resolver backed by Claude (the :strong tier unless `:model` is given).
  opts: :models tier overrides (config.edn [:anthropic :models]), :model"
  [client {:keys [models model]}]
  (let [model (or model (llm/model-for models :strong))]
    (fn [batch]
      (assoc (llm/complete-json client {:system     system-prompt
                                        :prompt     (resolver-prompt batch)
                                        :model      model
                                        :max-tokens 32000
                                        :effort     :medium
                                        :schema     reply-schema})
             :by model))))

;; A concept added by hand without a gloss gets one from the passages naming it.

(def ^:private gloss-system
  "You write the gloss of a concept in a personal knowledge base: one sentence
fixing its sense as the given passages use it, as a textbook glossary would
(\"A strategy profile from which no player gains by deviating alone.\").
Do not start with the concept's name; no citations.")

(defn llm-gloss
  "(fn [label passages] gloss) backed by Claude (the :common tier): the sense
of `label` in `passages` (texts), in one sentence. opts: :models"
  [client {:keys [models]}]
  (let [model (llm/model-for models :common)]
    (fn [label passages]
      (-> (llm/complete client {:system     gloss-system
                                :prompt     (str "Concept: " label "\n\nPassages:\n\n"
                                                 (str/join "\n\n" (map #(str "- " %) passages)))
                                :model      model
                                :max-tokens 300})
          str/trim))))

;;=============================================================================
;; Curation (human-written)
;;=============================================================================
;; Each op is a pure `stages` fn (db, args -> [tx-data ...]) plus a `!` that
;; applies it atomically and logs it. Ops that need a new concept take its id
;; as an argument, so the log replays to the same ids.

(defn- stages
  [db op args]
  (case op
    :rename
    (let [[id label] args, e (eid! db id)]
      ;; the old label stays an alias: it was a name for the concept
      [[[:db/add e :concept/label label] [:db/add e :concept/aliases (norm label)]
        [:db/add e :concept/by human]]])

    :set-gloss
    (let [[id gloss] args, e (eid! db id)]
      [[[:db/add e :concept/gloss gloss] [:db/add e :concept/by human]]])

    :merge
    (let [[into-id from-id] args
          into-e (eid! db into-id), from-e (eid! db from-id)
          from   (d/pull db [:concept/label :concept/aliases {:concept/broader [:db/id]}
                             {:concept/_broader [:db/id]}]
                         from-e)]
      (when (= into-e from-e) (throw (ex-info "Cannot merge a concept into itself" {:concept/id into-id})))
      [[[:db/retractEntity from-e]]
       (concat
        (for [a (distinct (conj (:concept/aliases from) (norm (:concept/label from))))]
          [:db/add into-e :concept/aliases a])
        ;; from's parents become into's; from's children point at into
        (for [{p :db/id} (:concept/broader from) :when (not= p into-e)]
          [:db/add into-e :concept/broader p])
        (for [{c :db/id} (:concept/_broader from) :when (not= c into-e)]
          [:db/add c :concept/broader into-e])
        ;; prerequisite claims (concept.prereq) of and on from are into's
        (for [attr [:prereq/of :prereq/needs]
              p    (d/q '[:find [?p ...] :in $ ?a ?c :where [?p ?a ?c]] db attr from-e)]
          [:db/add p attr into-e])
        ;; so are the exercises testing from (robertluo.fly-learner.exercises)
        (for [x (d/q '[:find [?x ...] :in $ ?c :where [?x :exercise/concepts ?c]] db from-e)]
          [:db/add x :exercise/concepts into-e])
        [[:db/add into-e :concept/by human]])])

    :split
    (let [[id aliases {new-id :concept/id :concept/keys [label gloss]}] args
          e        (eid! db id)
          aliases  (set (map norm aliases))
          own      (set (:concept/aliases (d/pull db [:concept/aliases] e)))
          _        (when-let [bad (seq (remove own aliases))]
                     (throw (ex-info "Not aliases of the concept" {:concept/id id :aliases bad})))
          broader  (map :db/id (:concept/broader (d/pull db [{:concept/broader [:db/id]}] e)))
          moved    (cond-> aliases (own (norm label)) (conj (norm label)))]
      [(for [a moved] [:db/retract e :concept/aliases a])
       (concat
        [(cond-> {:db/id "new" :concept/id new-id :concept/label label
                  :concept/aliases (vec (conj moved (norm label))) :concept/by human}
           gloss (assoc :concept/gloss gloss))
         [:db/add e :concept/by human]]
        ;; a sense split off keeps the original's place in the hierarchy
        (for [p broader] [:db/add "new" :concept/broader p]))])

    :assign
    (let [[alias id] args, a (norm alias), e (eid! db id), from (owner db a)
          ign (d/entid db [:ignored/keyword a])]
      (if (= from e)
        [[]]
        [(cond-> []
           from (conj [:db/retract from :concept/aliases a])
           ign  (conj [:db/retractEntity ign]))
         [[:db/add e :concept/aliases a] [:db/add e :concept/by human]]]))

    :ignore
    (let [[alias] args, a (norm alias), from (owner db a)]
      [(cond-> [{:ignored/keyword a}]
         from (conj [:db/retract from :concept/aliases a]))])

    :unignore
    (let [[alias] args]
      [(if-let [ign (d/entid db [:ignored/keyword (norm alias)])] [[:db/retractEntity ign]] [])])

    :add-broader
    (let [[id parent-id] args, e (eid! db id), p (eid! db parent-id)]
      (when (= e p) (throw (ex-info "A concept cannot be its own broader" {:concept/id id})))
      [[[:db/add e :concept/broader p] [:db/add e :concept/by human]]])

    :remove-broader
    (let [[id parent-id] args]
      [[[:db/retract (eid! db id) :concept/broader (eid! db parent-id)]]])

    :add
    (let [[{:concept/keys [id label gloss aliases]} node-ids] args
          names (distinct (cons (norm label) (map norm aliases)))]
      (when (str/blank? label) (throw (ex-info "A concept needs a name" {})))
      (doseq [a names]
        (when-let [o (owner db a)]
          (throw (ex-info (str "\"" a "\" already names " (:concept/label (d/pull db [:concept/label] o)))
                          {:alias a :concept/id (:concept/id (d/pull db [:concept/id] o))}))))
      ;; a name ignored before (too generic, say) names this concept now
      [(vec (for [a names :let [ign (d/entid db [:ignored/keyword a])] :when ign] [:db/retractEntity ign]))
       (into [(cond-> {:db/id "new" :concept/id id :concept/label (str/trim label)
                       :concept/aliases (vec names) :concept/by human}
                (not (str/blank? gloss)) (assoc :concept/gloss (str/trim gloss)))]
             ;; its mentions: the passages naming it carry its label as a keyword
             (for [nid node-ids :let [e (d/entid db [:node/id nid])] :when e]
               [:db/add e :node/keywords (norm label)]))])

    :remove
    (let [[ids {:keys [ignore?] :or {ignore? true}}] args
          es    (mapv #(eid! db %) ids)
          names (distinct (for [e es
                                :let [{:concept/keys [label aliases]} (d/pull db [:concept/label :concept/aliases] e)]
                                a (cons (norm label) aliases)]
                            a))]
      ;; claims of and on them go too (concept.prereq); exercises and narrower
      ;; concepts just lose the reference
      [(concat
        (for [e es, attr [:prereq/of :prereq/needs]
              p (d/q '[:find [?p ...] :in $ ?a ?c :where [?p ?a ?c]] db attr e)]
          [:db/retractEntity p])
        (for [e es] [:db/retractEntity e]))
       ;; their keywords name nothing from now on: a later resolve! leaves them alone
       (when ignore?
         (for [a names :when (not (d/entid db [:ignored/keyword a]))] {:ignored/keyword a}))])

    :retag
    (let [[node-ids from to] args, from (norm from), to (norm to)]
      [(vec (for [nid node-ids
                  :let [e (d/entid db [:node/id nid])]
                  :when (and e (some #{from} (:node/keywords (d/pull db [:node/keywords] e))))
                  tx [[:db/retract e :node/keywords from] [:db/add e :node/keywords to]]]
              tx))])))

(defn curate!
  "Apply curation `op` with `args` (see the fns below) atomically, logged as
  an edit by `by`."
  [conn by op & args]
  (apply-stages! conn (stages (d/db conn) op (vec args)) (edit op by (vec args))))

(defn rename!         [conn id label]  (curate! conn human :rename id label))
(defn set-gloss!      [conn id gloss]  (curate! conn human :set-gloss id gloss))
(defn merge!          "Merge concept `from-id` into `into-id`: aliases, parents, children."
  [conn into-id from-id]               (curate! conn human :merge into-id from-id))
(defn assign!         "Make keyword `alias` name concept `id` (from another concept or ignored)."
  [conn alias id]                      (curate! conn human :assign alias id))
(defn ignore!         [conn alias]     (curate! conn human :ignore alias))
(defn unignore!       "Back to unresolved: the next `resolve!` decides it."
  [conn alias]                         (curate! conn human :unignore alias))
(defn add-broader!    [conn id parent] (curate! conn human :add-broader id parent))
(defn remove-broader! [conn id parent] (curate! conn human :remove-broader id parent))

(defn retag!
  "On the nodes `node-ids`, keyword `from` becomes `to`. For a homonym, one
  keyword naming different ideas in different places (\"type\" of a player
  in a game, of a term in type theory): retag the nodes of one sense with a
  keyword naming it, which an existing concept has as alias, or the next
  `resolve!` gives one. See `keyword-nodes`."
  [conn node-ids from to]              (curate! conn human :retag (vec node-ids) from to))

(defn taken
  "The first of `names` another concept has: {:alias :concept/id :concept/label}, else nil."
  [db names]
  (some (fn [a] (when-let [o (owner db (norm a))]
                  (assoc (d/pull db [:concept/id :concept/label] o) :alias (norm a))))
        names))

(defn add!
  "A concept the model missed: {:concept/label, optional :concept/gloss and
  :concept/aliases (other names)}, tagged (its label as a keyword) on the
  nodes `node-ids` (see `naming-nodes`). Fails when one of its names is
  another concept's. Returns its id."
  [conn {:concept/keys [label] :as concept} node-ids]
  {:pre [(not (str/blank? label))]}
  (let [id (or (:concept/id concept) (random-uuid))]
    (curate! conn human :add (assoc concept :concept/id id) (vec node-ids))
    id))

(defn remove!
  "Not concepts: remove concepts `ids`, with the prerequisite claims of and on
  them; their names become ignored keywords (unless `:ignore? false`: the
  background they alone needed, which a source may still teach)."
  ([conn ids] (remove! conn ids {:ignore? true}))
  ([conn ids opts] (curate! conn human :remove (vec ids) opts)))

(defn background-of
  "Ids of the hidden concepts (background the sources assume, concept.prereq)
  that concepts `ids` need."
  [db ids]
  (d/q '[:find [?nid ...] :in $ [?id ...]
         :where [?c :concept/id ?id] [?p :prereq/of ?c] [?p :prereq/needs ?n]
         [?n :concept/origin :inferred] [?n :concept/id ?nid]]
       db ids))

(defn needed-by-none
  "Those of concepts `ids` no prerequisite claim names as needed."
  [db ids]
  (filterv (fn [id] (when-let [e (d/entid db [:concept/id id])]
                      (empty? (d/q '[:find [?p ...] :in $ ?n :where [?p :prereq/needs ?n]] db e))))
           ids))

(defn- phrase-re
  "Matches `term` as whole words, any case, a plural ending allowed."
  [term]
  (re-pattern (str "(?iu)(?<![\\p{L}\\p{N}])"
                   (str/join "\\s+" (map #(java.util.regex.Pattern/quote %) (str/split (str/trim term) #"\s+")))
                   "(?:s|es)?(?![\\p{L}\\p{N}])")))

(defn naming-nodes
  "Ids of the nodes whose text or title names one of `terms` (whole words,
  any case): the passages about a concept known by those names."
  [db terms]
  (let [res (mapv phrase-re (remove str/blank? terms))]
    (when (seq res)
      (->> (d/q '[:find ?id ?t ?x :where [?n :node/id ?id] [?n :node/title ?t] [(get-else $ ?n :node/text "") ?x]] db)
           (filter (fn [[_ t x]] (some #(or (re-find % t) (re-find % x)) res)))
           (mapv first)))))

^:rct/test
(comment
  (re-find (phrase-re "lambda  calculus") "The Lambda calculi and Lambda calculus.") ;=> "Lambda calculus"
  (re-find (phrase-re "type") "typed terms") ;=> nil
  (re-find (phrase-re "stream") "infinite streams.") ;=> "streams"
  )

(defn keyword-nodes
  "Ids of the nodes with keyword `kw`, of source `source-id` (any when nil)."
  [db kw source-id]
  (->> (d/q '[:find ?nid ?sid :in $ ?k :where [?n :node/keywords ?k] [?n :node/id ?nid]
              [?n :node/source ?s] [?s :source/id ?sid]]
            db (norm kw))
       (filter #(or (nil? source-id) (= source-id (second %))))
       (mapv first)))

(defn split!
  "Move `aliases` of concept `id` to a new concept (another sense of it), which
  keeps the original's broader parents. Returns the new concept's id."
  [conn id aliases {:concept/keys [label] :as new-concept}]
  {:pre [(not (str/blank? label))]}
  (let [new-id (or (:concept/id new-concept) (random-uuid))]
    (curate! conn human :split id (vec aliases) (assoc new-concept :concept/id new-id))
    new-id))

(defn edits
  "The edit log, oldest first."
  [db]
  (->> (d/q '[:find [(pull ?e [:db/id :edit/op :edit/by :edit/args :edit/at]) ...] :where [?e :edit/op]] db)
       (sort-by :db/id) ; entity ids grow with each transaction; :edit/at is only ms
       (mapv #(dissoc % :db/id))))
