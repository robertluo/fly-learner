(ns robertluo.fly-learner.ui.core.db
  "Application state: pure db -> db updater functions.

  Every function takes db (and optional args) and returns the new db.
  Effects are declared in views as effect maps and executed by dispatch!
  (robertluo.fly-learner.ui.core)."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.explore :as explore]
            [robertluo.fly-learner.explore.examples :as examples]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ui.core.graph :as graph]
            [robertluo.fly-learner.ui.core.layout :as layout]
            [robertluo.fly-learner.ui.core.viewport :as viewport]))

;;=============================================================================
;; State shape
;;=============================================================================

(def initial-state
  {:view            :home  ; :home (the map) | :cluster | :node | :keyword | :term | :ask | :concept | :concepts | :notebook | :study | :explore | :settings
   :universe        nil    ; every cluster (server.study/universe) and the map's layout, once fetched
   :cluster-id      nil    ; cluster shown in :cluster view
   :clusters        {}     ; cluster-id -> detail (server.study/cluster): next items in it
   :sources         []     ; [{:source/id :source/title :source/root {:node/id :node/summary}}]
   :node-id         nil    ; node shown in :node view
   :nodes           {}     ; node-id -> node (cache of fetched nodes)
   :keyword         nil    ; keyword shown in :keyword view
   :keyword-results {}     ; keyword -> [node-ref]
   :term            nil    ; a term of the learner's shown in :term view (ideas/term-key)
   :notebook        {:order :sources :highlights? false} ; the learner's notes as a book: arranged how, with what
   :named           {}     ; term -> [node-ref]: the passages naming it
   :concept-id      nil    ; concept shown in :concept view
   :concepts        {}     ; concept-id -> concept (see robertluo.fly-learner.concept.view/concept)
   :concept-index   nil    ; [concept-ref] by label, once fetched (:concepts view)
   :concept-filter  ""     ; text filtering the index
   :concept-form    {:label "" :aliases "" :gloss "" :adding? false} ; a concept being added by hand
   :graph           {:ix       nil  ; indexed concept graph (ui.core.graph/index), once fetched
                     :focus    nil  ; concept the shown graph grows from
                     :expanded #{}  ; concepts whose neighbours are shown
                     :scope    nil  ; #{concept-id}: show exactly these (a cluster), else focus + expanded
                     :pos      {}   ; id -> [x y] target positions
                     :from     {}   ; id -> [x y] positions the animation starts from
                     :t        1.0} ; animation progress, 0..1
   :learning        nil    ; what the learner knows (robertluo.fly-learner.server.study/overview), once fetched
   :quiz            {:status   nil  ; nil | :asking | :answering | :grading | :graded
                     :item     nil  ; item asked about (nil: the server picks what to study next)
                     :question nil  ; the question, without its answer
                     :response ""   ; the learner's answer being written
                     :result   nil} ; grading, model answer, scores before/after
   :ideas           {:all       nil   ; [idea with :idea/targets] once fetched (server.ideas/all)
                     :selection nil   ; {:anchor quote-anchor :x :y :top :vh}: selected text offered a
                                      ; reaction or a question; :ask {:question :status :answer} once asked
                     :draft     nil   ; {:anchors [..] :stance :text "" :links #{} :preview? :wiki}: an idea being written
                     :layer?    true} ; show my ideas on the concept graph
   :exploration-id  nil    ; exploration shown in :explore view (nil: the list): a uuid (kept) or an example's name
   :explore-states  {}     ; exploration-id -> its state (robertluo.fly-learner.explore/act)
   :explorations    {}     ; uuid -> exploration (robertluo.fly-learner.explore.knowledge/exploration)
   :exploration-list nil   ; [exploration without spec], newest first, once fetched
   :exploring       nil    ; an exploration being made: {:node id} or {:previous id :feedback str}
   :explore-feedback ""    ; what the learner asks to change in the one shown
   :ask-id          nil    ; answer shown in :ask view
   :asking          nil    ; question waiting for its answer (:ask view, no :ask-id yet)
   :asks            {}     ; ask-id -> ask (see robertluo.fly-learner.ask/view)
   :ask-history     []     ; [{:ask/id :ask/question :ask/title :ask/at}], newest first
   :search-hits     {}     ; question -> [node-ref with :node/path] (full-text, no model)
   :box             {:q "" :open? false} ; the top box: what is typed, suggestions shown?
   :goto            {}     ; typed text -> {:concepts [ref ..] :exercises [..]} it names
   :doc             {:page      nil   ; manual page (nil: follow the node)
                     :pages     {}    ; source-id -> page count
                     :aspect    {}    ; source-id -> width/height of a page (pages not yet drawn)
                     :highlight nil   ; {:key [url page node-id] :aspect w/h :rects [{:x :y :w :h}]}
                     :text-ready #{}  ; "url#page" whose text layer has been laid out
                     :draft     nil   ; the page number being typed in the toolbar, nil when not typing
                     :zoom      1.0   ; page width / the pane's
                     :placed    {}}   ; quotes key (doc-quotes-key) -> {idea-id [{:x :y :w :h}]}
   :keypoints       {}     ; source-id -> [{:page :para :node :quote :label}]: key sentences (robertluo.fly-learner.keypoints)
   :source-only?    false  ; split views: the left pane hidden, the source (its document, the graph) full width
   :split           0.5    ; split views: the left pane's share of the width (kept in localStorage)
   :cards           {:all? false  ; drill-down summaries read in full (kept in localStorage)
                     :open #{}}   ; node ids whose summary is read in full (the others not, when not all?)
   :split-drag      nil    ; that share while the divider is being dragged
   :fullscreen?     false  ; the browser is full screen, showing only the pane (map, graph, source document)
   :map-view        {}     ; the map's pan and zoom (robertluo.fly-learner.ui.core.viewport)
   :settings        {:models   nil   ; models by tier and providers (server.settings/view), once fetched
                     :draft    nil   ; those being edited (`models-draft`), saved with `models-request`
                     :listed   {}    ; provider name -> {:status :models :problem}: what it serves
                     :tried    {}    ; tier -> {:status :ms :reply :problem}: a tiny request to its model
                     :ingests  nil   ; ingest jobs of the server's run, newest first, with their logs
                     :prompts  nil   ; [{:prompt/id :prompt/group :prompt/title :prompt/about :prompt/default
                                     ;   :prompt/text :prompt/edited?}], once fetched
                     :uploads  []    ; files dropped: {:id :file :status (:uploading :ready :failed) :error
                                     ;   :candidates [describe + :include? :skip #{part}]}
                     :open     nil   ; the prompt being edited
                     :drafts   {}    ; prompt-id -> the text being written
                     :drag?    false}; files dragged over the drop zone
   :loading?        false
   :error           nil})  ; {:code :reason} or nil

(defn current-node [db]
  (get-in db [:nodes (:node-id db)]))

;;=============================================================================
;; Loading / errors
;;=============================================================================

(defn set-loading [db] (assoc db :loading? true :error nil))

(defn set-error [db error]
  (-> db
      (assoc :loading? false :asking nil
             :error (if (map? error) error {:code :unknown :reason (str error)}))
      ;; a failed model call: back to where the learner can try again
      (update-in [:quiz :status] {:asking nil :grading :answering})
      (assoc :exploring nil)
      (assoc-in [:concept-form :adding?] false)
      (cond-> (= :asking (get-in db [:ideas :selection :ask :status]))
        (update-in [:ideas :selection] dissoc :ask))))

(defn clear-error [db] (assoc db :error nil))

;;=============================================================================
;; Navigation
;;=============================================================================

(defn go-home [db]
  (assoc db :view :home :node-id nil :keyword nil :asking nil :cluster-id nil))

(declare cluster-of graph-scope)

(defn open-cluster
  "Show cluster `id`: what it covers, and its concepts in the graph."
  [db id]
  (let [db (assoc db :view :cluster :cluster-id id :asking nil :node-id nil :keyword nil
                  :loading? (nil? (:universe db)))]
    (if-let [c (cluster-of db id)]
      (graph-scope db (:concepts c))
      db)))

(defn open-node
  "Show node `id`; the source pane follows the node's page again."
  [db id]
  (-> db
      (assoc :view :node :node-id id :keyword nil :asking nil)
      (assoc-in [:doc :page] nil)
      (assoc :loading? (not (contains? (:nodes db) id)))))

(defn open-keyword [db kw]
  (-> db
      (assoc :view :keyword :keyword kw :asking nil)
      (assoc :loading? (not (contains? (:keyword-results db) kw)))))

(defn open-term
  "A term of the learner's: a `[[..]]` link naming no concept."
  [db t]
  (let [k (ideas/term-key t)]
    (-> db
        (assoc :view :term :term k :asking nil)
        (assoc :loading? (not (contains? (:named db) k))))))

(defn named-fetched [db t nodes]
  (-> db (assoc-in [:named t] (vec nodes)) (assoc :loading? false)))

(defn open-ask
  "Show the stored answer `id`."
  [db id]
  (assoc db :view :ask :ask-id id :asking nil :node-id nil :keyword nil
         :loading? (not (contains? (:asks db) id))))

(defn ask-submitted
  "Show `question` as asked; its answer comes with ask-created."
  [db question]
  (assoc db :view :ask :ask-id nil :asking question :node-id nil :keyword nil :error nil))

(declare show-in-graph open-study)

(defn open-concept
  "Show concept `id`; the graph keeps its shape when `id` is already in it,
  else it restarts from `id`."
  [db id]
  (-> db
      (assoc :view :concept :concept-id id :asking nil
             :loading? (not (contains? (:concepts db) id)))
      (show-in-graph id)))

(defn open-concepts [db]
  (assoc db :view :concepts :asking nil :loading? (nil? (:concept-index db))))

(defn open-notebook [db] (assoc db :view :notebook :asking nil))

(defn notebook-order [db order] (cond-> db (#{:sources :newest} order) (assoc-in [:notebook :order] order)))

(defn notebook-highlights [db on?] (assoc-in db [:notebook :highlights?] (boolean on?)))

(defn knowledge-changed
  "Concepts or topics were added or removed: what was fetched of them is
  fetched again (the index, the graph, the map, what to study)."
  [db]
  (-> db
      (assoc :concept-index nil :concepts {} :clusters {} :learning nil)
      ;; what the learner's [[..]] links name is resolved again
      (assoc-in [:ideas :all] nil)
      (assoc-in [:graph :ix] nil)
      (cond-> (:universe db) (assoc-in [:universe :stale?] true))))

(defn concept-form-typed
  "The learner typed `v` in field `k` (:label, :aliases, :gloss) of the form adding a concept."
  [db k v]
  (assoc-in db [:concept-form k] v))

(defn concept-form-value
  "What the form adds: {:concept/label :concept/gloss :concept/aliases}, nil without a name."
  [{{:keys [label aliases gloss]} :concept-form}]
  (when-not (str/blank? label)
    (cond-> {:concept/label (str/trim label)
             :concept/aliases (vec (remove str/blank? (map str/trim (str/split (or aliases "") #"[,;]"))))}
      (not (str/blank? gloss)) (assoc :concept/gloss (str/trim gloss)))))

^:rct/test
(comment
  (concept-form-value {:concept-form {:label " Tabulation " :aliases "table lookup; memo table, " :gloss ""}})
  ;=> {:concept/label "Tabulation" :concept/aliases ["table lookup" "memo table"]}
  (concept-form-value {:concept-form {:label "  " :aliases "x"}}) ;=> nil
  )

(defn concept-adding [db] (-> db (assoc-in [:concept-form :adding?] true) (assoc :error nil)))

(defn concept-added
  "Concept `c` (its page) was added: show it, the form emptied."
  [db {:concept/keys [id] :as c}]
  (-> (knowledge-changed db)
      (assoc :concept-form (:concept-form initial-state))
      (assoc-in [:concepts id] c)
      (open-concept id)))

(defn concept-removed
  "Concept `id` was removed: back to the index of the others."
  [db id]
  (-> (knowledge-changed db)
      (update :concepts dissoc id)
      (assoc :view :concepts :concept-id nil :loading? true)))

(defn current-concept [db]
  (get-in db [:concepts (:concept-id db)]))

(defn set-concept-filter [db s] (assoc db :concept-filter s))

(defn filtered-concepts
  "The index entries whose label or gloss contains every word of the filter."
  [{:keys [concept-index concept-filter]}]
  (let [words (re-seq #"\S+" (str/lower-case (or concept-filter "")))]
    (filterv (fn [{:concept/keys [label gloss]}]
               (let [text (str/lower-case (str label " " gloss))]
                 (every? #(str/includes? text %) words)))
             concept-index)))

(defn current-ask [db]
  (get-in db [:asks (:ask-id db)]))

(defn open-explore
  "Show exploration `id` (nil: the list of them)."
  [db id]
  (assoc db :view :explore :exploration-id id :asking nil :node-id nil :keyword nil
         :explore-feedback ""
         :loading? (and (uuid? id) (not (contains? (:explorations db) id)))))

(def example-specs
  "name -> spec (normalized) of the examples written by hand."
  (update-vals examples/all explore/normalize))

(defn exploration-spec
  "The spec of exploration `id`: a kept one (once fetched) or an example."
  [db id]
  (if (uuid? id)
    (get-in db [:explorations id :exploration/spec])
    (get example-specs id)))

(defn current-exploration [db]
  (let [id (:exploration-id db)] (when (uuid? id) (get-in db [:explorations id]))))

(defn exploration-fetched [db x]
  (if x
    (-> db (assoc-in [:explorations (:exploration/id x)] x) (assoc :loading? false))
    (set-error db {:code :not-found :reason "Exploration not found"})))

(defn exploration-list-fetched [db xs] (assoc db :exploration-list (vec xs)))

(defn explore-node
  "Make an exploration of node `node-id` (its proposal's pitch)."
  [db node-id]
  (assoc db :exploring {:node node-id} :error nil))

(defn explore-feedback [db s] (assoc db :explore-feedback s))

(defn explore-remake
  "Make exploration `id` again, as the learner asks in the feedback."
  [db id]
  (assoc db :exploring {:previous id :feedback (str/trim (:explore-feedback db))} :error nil))

(defn exploration-made
  "A new exploration arrived: keep it, show it; the node it is about and the
  list are fetched again (they now include it)."
  [db x]
  (let [node (get-in x [:exploration/node :node/id])]
    (-> db
        (assoc :exploring nil :explore-feedback "" :exploration-list nil)
        (update :nodes dissoc node)
        (assoc-in [:explorations (:exploration/id x)] x)
        (open-explore (:exploration/id x)))))

(defn exploration-deleted [db id]
  (let [node (get-in db [:explorations id :exploration/node :node/id])]
    (-> db
        (update :explorations dissoc id)
        (update :nodes dissoc node)
        (assoc :exploration-list nil)
        (open-explore nil))))

(defn exploration-state
  "The state exploration `id` is in: where the learner left it, else its start."
  [db id]
  (or (get-in db [:explore-states id])
      (some-> (exploration-spec db id) explore/init-state)))

(defn explored
  "The learner did `action` (robertluo.fly-learner.explore/act) in exploration `id`."
  [db id action]
  (if-let [spec (exploration-spec db id)]
    (assoc-in db [:explore-states id] (explore/act spec (exploration-state db id) action))
    db))

(defn apply-route
  "Apply a parsed URL ({:view .. :id .. :keyword .. :page ..}, see history) to db."
  [db {:keys [view id keyword term page]}]
  (case view
    :node    (cond-> (open-node db id) page (assoc-in [:doc :page] (max 1 page)))
    :keyword (open-keyword db keyword)
    :term    (open-term db term)
    :ask      (open-ask db id)
    :concept  (open-concept db id)
    :cluster  (open-cluster db id)
    :concepts (open-concepts db)
    :notebook (open-notebook db)
    :study    (open-study db)
    :explore  (open-explore db id)
    :settings (assoc db :view :settings :asking nil)
    (go-home db)))

;;=============================================================================
;; Pull results
;;=============================================================================

(defn sources-fetched [db sources]
  (assoc db :sources (vec sources) :loading? (and (= :node (:view db))
                                                  (not (current-node db)))))

(defn node-fetched [db node]
  (if node
    (-> db
        (assoc-in [:nodes (:node/id node)] node)
        (assoc :loading? false))
    (set-error db {:code :not-found :reason "Node not found"})))

(defn- history-entry [ask]
  (select-keys ask [:ask/id :ask/question :ask/title :ask/at]))

(defn ask-created
  "An answer to `question` arrived: cache it, list it, and show it if the
  user is still waiting for it."
  [db question ask]
  (let [db (-> db
               (assoc-in [:asks (:ask/id ask)] ask)
               (update :ask-history (fn [h] (into [(history-entry ask)]
                                                  (remove #(= (:ask/id ask) (:ask/id %)))
                                                  h))))]
    (if (and (= :ask (:view db)) (= question (:asking db)))
      (assoc db :ask-id (:ask/id ask) :asking nil)
      db)))

(defn ask-fetched [db ask]
  (if ask
    (-> db
        (assoc-in [:asks (:ask/id ask)] ask)
        (assoc :loading? false))
    (set-error db {:code :not-found :reason "Answer not found"})))

(defn history-fetched [db asks]
  (assoc db :ask-history (vec asks)))

(defn box-typed
  "The learner typed `q` in the top box: suggest what it names."
  [db q]
  (assoc db :box {:q q :open? true}))

(defn box-open [db open?] (assoc-in db [:box :open?] (boolean open?)))

(defn box-cleared [db] (assoc db :box {:q "" :open? false}))

(defn goto-query
  "The top box's text to look up, nil when too short or closed."
  [db]
  (let [{:keys [q open?]} (:box db)
        q (str/trim (str q))]
    (when (and open? (<= 2 (count q))) q)))

(defn goto-fetched [db q r] (assoc-in db [:goto q] r))

(defn box-suggestions
  "What the top box's text names: {:concepts :exercises}, nil when nothing
  (yet): the last fetched for a prefix of it while its own is on the way."
  [db]
  (when-let [q (goto-query db)]
    (let [r (or (get-in db [:goto q])
                (some #(get-in db [:goto (subs q 0 %)]) (range (dec (count q)) 1 -1)))]
      (when (or (seq (:concepts r)) (seq (:exercises r))) r))))

(defn search-fetched [db q hits]
  (assoc-in db [:search-hits q] (vec hits)))

(defn concept-fetched [db concept]
  (if concept
    (-> db
        (assoc-in [:concepts (:concept/id concept)] concept)
        (assoc :loading? false))
    (set-error db {:code :not-found :reason "Concept not found"})))

(defn concept-index-fetched [db refs]
  (assoc db :concept-index (vec refs) :loading? false))

;;=============================================================================
;; Learning: scores and quizzes
;;=============================================================================

(defn concept-item [id] [:concept id])

(defn relation-item
  "Item of the relation between concepts `a` and `b` (as robertluo.fly-learner.learning.concepts)."
  [a b]
  (let [[a b] (sort-by str [a b])] [:relation a b]))

(defn item-score [db item] (get-in db [:learning :scores item] 0))

(defn learning-fetched [db overview]
  (assoc db :learning overview))

(defn open-study [db]
  (assoc db :view :study :asking nil :loading? (nil? (:learning db))))

(defn quiz-start
  "Ask a question on `item` (nil: what to study next), where the learner is:
  the study page shows it in place, other pages dock it at the top of the
  left pane, where it stays while the learner reads on to answer it.
  `exercise`: the id of one of the item's book exercises to answer."
  ([db item] (quiz-start db item nil))
  ([db item exercise]
   (assoc db :quiz {:status :asking :item item :exercise exercise :question nil :response "" :result nil
                    :folded? false})))

(defn exercise-grade
  "The best grade of the learner's answers to book exercise `id`, nil if none."
  [db id]
  (get-in db [:learning :exercises id]))

(defn quiz-fold
  "Fold the docked quiz to one line (`folded?`), or open it again."
  [db folded?]
  (assoc-in db [:quiz :folded?] (boolean folded?)))

(defn question-created [db question]
  (if (= :asking (get-in db [:quiz :status]))
    (update db :quiz assoc :status :answering :question question :item (:question/item question))
    db))

(defn quiz-typed [db s] (assoc-in db [:quiz :response] s))

(defn quiz-submitted [db] (assoc-in db [:quiz :status] :grading))

(defn quiz-graded
  "The grading arrived: show it, and take the new scores it carries."
  [db result]
  (cond-> (-> (update db :quiz assoc :status :graded :result result)
              ;; scores moved: clusters' next items and scores are fetched again
              (assoc :clusters {})
              (cond-> (:universe db) (assoc-in [:universe :stale?] true)))
    (:overview result) (assoc :learning (:overview result))))

(defn quiz-close [db] (assoc-in db [:quiz :status] nil))

;;=============================================================================
;; Ideas: the learner's own voice on the knowledge
;;=============================================================================

(defn concept-anchor [id] {:anchor/kind :concept :concept/id id})

(defn node-anchor [{:node/keys [id title source]}]
  {:anchor/kind :node :node/id id :node/title title :source/id (:source/id source)})

(defn ideas-fetched [db all] (assoc-in db [:ideas :all] (vec all)))

(defn idea-added
  "A new idea was stored: list it, and close the selection and draft it came from."
  [db idea]
  (-> db
      (update-in [:ideas :all] (fnil conj []) idea)
      (update :ideas assoc :selection nil :draft nil)))

(defn idea-updated
  "Idea `idea` was rewritten: listed as it is now, the draft closed."
  [db {:idea/keys [id] :as idea}]
  (-> db
      (update-in [:ideas :all] (fn [all] (mapv #(if (= id (:idea/id %)) idea %) all)))
      (assoc-in [:ideas :draft] nil)))

(defn idea-removed [db id]
  (update-in db [:ideas :all] (fn [all] (vec (remove #(= id (:idea/id %)) all)))))

(defn- target-keys [idea] (set (map :key (:idea/targets idea))))

(defn ideas-on
  "Ideas about any target in `keys` (e.g. #{[:concept id]}), oldest first."
  [db keys]
  (filterv #(some keys (target-keys %)) (get-in db [:ideas :all])))

(defn- on-doc-page?
  "Is `idea` on text selected on the source page `page-key` ([:page source-id page]),
  whatever paragraph that text was found in?"
  [page-key idea]
  (some #(and (= :page (:field %)) (= page-key [:page (:source/id %) (:page %)])) (:idea/anchors idea)))

(defn ideas-in-node
  "Ideas on node `id`, on anything inside it, or on the source page `page-key`
  ([:page source-id page]) shown beside it (marked there). Bookmarks are
  not ideas to read here (see bookmarks)."
  [db id page-key]
  (filterv #(and (not= :bookmark (:idea/stance %))
                 (or (some #{[:node id] page-key} (target-keys %))
                     (some #{id} (:idea/within %))
                     (on-doc-page? page-key %)))
           (get-in db [:ideas :all])))

(declare doc-page)

;;=============================================================================
;; Bookmarks: where to go on reading (ideas of stance :bookmark on a node)
;;=============================================================================

(defn bookmarks
  "The learner's bookmarks, newest first, on nodes still there:
  [{:idea/id :node-id :title :page :source/id :source/title :at}], page the
  source page it was made on (else the node's)."
  ([db] (bookmarks db nil))
  ([db source-id]
   (->> (get-in db [:ideas :all])
        (filter #(= :bookmark (:idea/stance %)))
        (keep (fn [{:idea/keys [id anchors targets at]}]
                (when-let [t (first (filter #(= :node (:kind %)) targets))]
                  {:idea/id id :node-id (second (:key t)) :title (:title t)
                   :page (or (:page (first anchors)) (:page t))
                   :source/id (:source/id t) :source/title (:source/title t) :at at})))
        (filter #(or (nil? source-id) (= source-id (:source/id %))))
        (sort-by :at >)
        vec)))

(defn bookmark-on
  "The bookmark on node `id`, or nil."
  [db id]
  (some #(when (= id (:node-id %)) %) (bookmarks db)))

(defn bookmark-anchor
  "Anchor of a bookmark on `node` at the source page shown."
  [db node]
  (assoc (node-anchor node) :page (doc-page db)))

(defn open-bookmark
  "Go on reading at bookmark `b`: its node, the source at its page."
  [db {:keys [node-id page]}]
  (cond-> (open-node db node-id)
    page (assoc-in [:doc :page] page)))

(defn ideas-about-concept
  "Ideas on concept `id` and on its relations."
  [db id]
  (filterv #(some (fn [[kind a b]] (or (= [kind a] [:concept id])
                                       (and (= :relation kind) (or (= a id) (= b id)))))
                  (target-keys %))
           (get-in db [:ideas :all])))

;;=============================================================================
;; The notebook: the learner's notes as a book
;;=============================================================================

(defn- notebook-ideas
  "What goes in the notebook: what the learner wrote (and highlights, if asked)."
  [db]
  (let [hl? (get-in db [:notebook :highlights?])]
    (filter #(or (:idea/text %) (and hl? (= :highlight (:idea/stance %)))) (get-in db [:ideas :all]))))

(defn notebook
  "The learner's notes as a book: [{:title :source/id :chapters [{:title
  :entries [idea ..]}]}]. As the sources go (`:order :sources`): by source,
  then its chapters, in reading order; notes on concepts and relations
  after, by what they are about. Or one part, newest first (`:newest`)."
  [db]
  (let [ideas (notebook-ideas db)]
    (if (= :newest (get-in db [:notebook :order]))
      (if (seq ideas) [{:title nil :chapters [{:title nil :entries (vec (sort-by (comp - :idea/at) ideas))}]}] [])
      (let [{placed true other false} (group-by (comp some? :idea/place) ideas)
            chapter #(get-in % [:idea/place :path 1])
            place-order (juxt #(get-in % [:idea/place :order]) #(or (get-in % [:idea/place :page]) 0) :idea/at)]
        (cond-> (->> placed
                     (group-by #(get-in % [:idea/place :source/id]))
                     (map (fn [[sid es]]
                            (let [es (sort-by place-order es)]
                              {:source/id sid
                               :title     (get-in (first es) [:idea/place :source/title])
                               :chapters  (->> (partition-by chapter es)
                                               (mapv (fn [es] {:title (:node/title (chapter (first es)))
                                                               :entries (vec es)})))})))
                     (sort-by :title)
                     vec)
          (seq other)
          (conj {:title "On concepts and relations"
                 :chapters [{:title nil :entries (vec (sort-by #(str/lower-case (or (:title (first (:idea/targets %))) "")) other))}]}))))))

(defn entry-where
  "Where an idea of the notebook is: {:node/id :title :page}, the passage
  and its section (the paragraph's own title is its first words), or nil."
  [{:idea/keys [place]}]
  (when place
    (let [path    (rest (:path place))
          section (last (remove #(= :paragraph (:node/kind %)) path))]
      {:node/id (:node/id (last path)) :title (:node/title section) :page (:page place)})))

(def ^:private stance-word {:agree "Agree" :restate "In my words" :disagree "Disagree"})

(defn notebook-markdown
  "The notebook as one Markdown document: sources and chapters as
  headings, each note under the passage it quotes; `[[..]]` links kept."
  [db]
  (let [quote-of (fn [i] (some :quote (:idea/anchors i)))
        entry (fn [i]
                (let [{:keys [title page]} (entry-where i)
                      q     (quote-of i)
                      where (str/join ", " (remove nil? [(when page (str "p. " page)) title]))
                      about (when-not (:idea/place i) (str/join ", " (map :title (:idea/targets i))))]
                  (str/join "\n\n"
                            (remove str/blank?
                                    [(when q (str "> " (str/replace (str/trim q) #"\s*\n\s*" " ")
                                                  (when (seq where) (str "\n>\n> — " where))))
                                     (when (and (not q) (seq where)) (str "*" where "*"))
                                     (when (seq about) (str "*On " about "*"))
                                     (when-let [t (:idea/text i)]
                                       (if-let [w (stance-word (:idea/stance i))] (str "**" w ".** " t) t))]))))]
    (str "# Notebook\n"
         (str/join (for [{:keys [title chapters]} (notebook db)]
                     (str (when title (str "\n## " title "\n"))
                          (str/join (for [{ct :title es :entries} chapters]
                                      (str (when ct (str "\n### " ct "\n"))
                                           (str/join (for [i es] (str "\n" (entry i) "\n"))))))))))))

;;=============================================================================
;; What the learner's notes say: derived from them, never stored
;;=============================================================================

(defn- note-ref
  "What term `t`, written in `idea`, names: {:concept c} when a concept,
  else the learner's own {:term key :label t}."
  [idea t]
  (if-let [c (get (:idea/refs idea) t)]
    {:concept c}
    {:term (ideas/term-key t) :label t}))

(defn note-ref-key
  "[:concept id] or [:term key] of a `note-ref`."
  [r]
  (if-let [c (:concept r)] [:concept (:concept/id c)] [:term (:term r)]))

(defn- notes [db] (filter :idea/text (get-in db [:ideas :all])))

(defn my-terms
  "The learner's own terms: {key {:term key :label t :notes [idea ..]}},
  the `[[..]]` links of their notes naming no concept."
  [db]
  (reduce (fn [m i]
            (reduce (fn [m t]
                      (let [r (note-ref i t)]
                        (cond-> m
                          (:term r) (update (:term r) #(update (or % r) :notes (fnil conj []) i)))))
                    m (ideas/wikilinks (:idea/text i))))
          {} (notes db)))

(defn my-claims
  "The prerequisites the learner's notes state: [{:of ref :needs ref :notes
  [idea ..]}], refs as `note-ref`, once each."
  [db]
  (->> (for [i (notes db), [a b] (ideas/claims (:idea/text i))] [(note-ref i a) (note-ref i b) i])
       (reduce (fn [m [a b i]]
                 (update m [(note-ref-key a) (note-ref-key b)] #(update (or % {:of a :needs b}) :notes (fnil conj []) i)))
               {})
       vals
       (sort-by (comp :idea/at first :notes))
       vec))

(defn in-my-notes
  "What the learner's notes say of `k` ([:concept id] or [:term key]):
  {:needs [ref ..] :needed-by [ref ..] :notes [idea ..]}, the notes naming it."
  [db k]
  (let [cs (my-claims db)]
    {:needs     (vec (for [{:keys [of needs]} cs :when (= k (note-ref-key of))] needs))
     :needed-by (vec (for [{:keys [of needs]} cs :when (= k (note-ref-key needs))] of))
     :notes     (filterv (fn [i] (some #(= k (note-ref-key (note-ref i %))) (ideas/wikilinks (:idea/text i))))
                         (notes db))}))

(defn reaction
  "The learner's bare agree/disagree (no text) on exactly `anchor`, or nil."
  [db anchor]
  (some #(when (and (#{:agree :disagree} (:idea/stance %)) (nil? (:idea/text %))
                    (= [anchor] (:idea/anchors %)))
           %)
        (get-in db [:ideas :all])))

(defn quote-marks
  "Marks for the `field` text of node `id`: quotes of ideas about it."
  [db id field]
  (for [i (ideas-on db #{[:node id]})
        a (:idea/anchors i)
        :when (and (= :quote (:anchor/kind a)) (= field (:field a)))]
    {:selector a :mark i}))

(defn selection-made
  "Text was selected: `sel` {:anchor quote-anchor :x :y :top :vh}, where to
  offer a reaction (viewport pixels: x centre, y bottom, top of the
  selection; vh the viewport's height)."
  [db sel]
  (assoc-in db [:ideas :selection] (select-keys sel [:anchor :x :y :top :vh])))

(defn selection-cleared
  "The selection went away; a note or a question about it keeps it."
  [db]
  (cond-> db
    (not (or (get-in db [:ideas :draft :from-selection?])
             (get-in db [:ideas :selection :ask])))
    (assoc-in [:ideas :selection] nil)))

(defn selection-ask
  "Ask the assistant `question` about the selected text."
  [db question]
  (let [q (str/trim (str question))]
    (cond-> db
      (and (seq q) (get-in db [:ideas :selection]))
      (assoc-in [:ideas :selection :ask] {:question q :status :asking}))))

(defn selection-answered
  "The answer to `question` about `anchor` arrived: listed with the other
  asks, and shown if that selection is still asked about."
  [db anchor question ask]
  (let [db (ask-created db question ask)
        {a :anchor {q :question} :ask} (get-in db [:ideas :selection])]
    (cond-> db
      (and (= anchor a) (= question q))
      (update-in [:ideas :selection :ask] assoc :status :answered :answer ask))))

(defn answer-as-note
  "Text for a note kept from an answer: the question, then the answer
  without its citations (they point into the answer's own sources)."
  [question answer]
  (let [s (str question "\n\n" (-> answer (str/replace #"\[\d+\]" "") (str/replace #"[ \t]+([.,;:])" "$1")))]
    (if (> (count s) ideas/max-text) (str (subs s 0 (dec ideas/max-text)) "…") s)))

(defn draft-start
  "Write a note about `anchors` (starting from `text`)."
  ([db anchors from-selection?] (draft-start db anchors from-selection? ""))
  ([db anchors from-selection? text]
   (assoc-in db [:ideas :draft] {:anchors (vec anchors) :stance :note :text text :links #{}
                                 :from-selection? from-selection?})))

(def draft-stances
  "What the learner may write: one stance a note."
  [:note :agree :restate :disagree])

(defn draft-stance [db stance]
  (cond-> db (some #{stance} draft-stances) (assoc-in [:ideas :draft :stance] stance)))

(defn draft-preview [db on?] (assoc-in db [:ideas :draft :preview?] (boolean on?)))

(defn wiki-term
  "The `[[` link being typed in `text` at `caret`: {:from i :term t}, i where
  the term starts (after the brackets), t what is typed of it; nil when the
  caret is not inside an open link."
  [text caret]
  (let [before (subs text 0 (min (count text) (max 0 (or caret 0))))
        i      (str/last-index-of before "[[")]
    (when i
      (let [t (subs before (+ i 2))]
        (when-not (re-find #"[\[\]\n|]" t)
          {:from (+ i 2) :term t})))))

(defn draft-edit
  "Rewrite `idea`: a draft holding what it says now."
  [db {:idea/keys [id anchors stance text links]}]
  (assoc-in db [:ideas :draft] {:editing id :anchors (vec anchors) :stance stance :text (or text "")
                                :links (set links) :from-selection? false}))

(defn draft-typed
  "The note's text is `s`, the caret at `caret` (where a `[[` link may be
  being typed, `wiki-term`)."
  ([db s] (draft-typed db s nil))
  ([db s caret]
   (update-in db [:ideas :draft] assoc :text s :wiki (when caret (wiki-term s caret)))))

(defn draft-wiki-closed [db] (assoc-in db [:ideas :draft :wiki] nil))

(defn wiki-query
  "The `[[` term being typed to look up, nil when none."
  [db]
  (let [t (some-> (get-in db [:ideas :draft :wiki :term]) str/trim)]
    (when (seq t) t)))

(defn wiki-suggestions
  "Concepts the `[[` term being typed may name (refs), the last fetched for
  a prefix of it while its own is on the way."
  [db]
  (when-let [q (wiki-query db)]
    (some #(seq (get-in db [:goto (subs q 0 %) :concepts])) (range (count q) 0 -1))))

(defn wiki-picked
  "The `[[` link being typed becomes `[[label]]`: [db caret], the caret after
  the link."
  [db label]
  (let [{:keys [text wiki]} (get-in db [:ideas :draft])]
    (if-let [{:keys [from term]} wiki]
      (let [end   (+ from (count term))
            after (subs text end)
            after (if (str/starts-with? after "]]") (subs after 2) after)
            head  (str (subs text 0 from) label "]]")]
        [(update-in db [:ideas :draft] assoc :text (str head after) :wiki nil) (count head)])
      [db nil])))

(defn draft-link
  "Link the note being written to idea `id`, or unlink it."
  [db id]
  (update-in db [:ideas :draft :links] #(if (contains? % id) (disj % id) (conj % id))))

(defn draft-cancel [db] (update db :ideas assoc :draft nil :selection nil))

(defn ideas-layer-toggle [db] (update-in db [:ideas :layer?] not))

(defn- graph-ideas
  "Ideas placed on the drawn graph: around their concept, or at the middle of
  their relation. {:ideas [{:idea .. :x :y}] :links [[from-xy to-xy] ..]}."
  [db nodes edges]
  (when (get-in db [:ideas :layer?])
    (let [at-node (into {} (map (juxt #(vector :concept (:id %)) identity)) nodes)
          at-edge (into {} (map (juxt #(relation-item (:from %) (:to %)) identity)) edges)
          placed  (->> (get-in db [:ideas :all])
                       (keep (fn [i] (some (fn [k] (when (or (at-node k) (at-edge k)) [k i])) (target-keys i))))
                       (group-by first)
                       (mapcat (fn [[k kis]]
                                 (map-indexed
                                  (fn [n [_ i]]
                                    (if-let [{:keys [x y r]} (at-node k)]
                                      (let [a (+ -0.9 (* 0.55 n)), d (+ r 14)]
                                        {:idea i :x (+ x (* d (Math/cos a))) :y (+ y (* d (Math/sin a)))})
                                      (let [{:keys [x1 y1 x2 y2]} (at-edge k)]
                                        {:idea i :x (+ (/ (+ x1 x2) 2) (* 12 n)) :y (- (/ (+ y1 y2) 2) 8)})))
                                  kis)))
                       vec)
          by-id   (into {} (map (juxt (comp :idea/id :idea) identity)) placed)]
      {:ideas placed
       :links (vec (for [link (ideas/links (map :idea placed))
                         :let [[a b] (map by-id link)]]
                     [[(:x a) (:y a)] [(:x b) (:y b)]]))})))

;;=============================================================================
;; Concept graph
;;=============================================================================

(def ^:private max-label-scale
  "Labels grow with the zoom-out up to this factor; the layout keeps label
  boxes apart at this size, so they never overlap."
  1.35)

(defn- label-width
  "Width a label may take in the graph (12px text at the largest scale), roughly."
  [label]
  (* max-label-scale (+ 14 (* 6.8 (count label)))))

(defn- shown-positions
  "Positions as drawn now (mid-animation too)."
  [{:keys [from pos t]}]
  (layout/tween from pos t))

(defn- graph-visible
  "Concepts the graph shows: its scope (a cluster), else the focus and the
  neighbours of expanded concepts."
  [{:keys [ix focus expanded scope]}]
  (cond
    (not ix) #{}
    scope    (into #{} (filter #(get-in ix [:concepts %])) scope)
    focus    (graph/visible ix focus expanded)
    :else    #{}))

(defn graph-relayout
  "Lay out what is visible now. Concepts already shown start where they are
  drawn; new ones grow out of the concept that revealed them."
  [db]
  (let [{:keys [ix expanded scope] :as g} (:graph db)
        vis (graph-visible g)]
    (if (empty? vis)
      db
      (let [shown (select-keys (shown-positions g) vis)
            seeds (if scope
                    {}
                    (into {} (for [id vis :when (not (shown id))]
                               [id (graph/revealer ix vis expanded id)])))]
        (update db :graph assoc
                :pos  (layout/layout vis (graph/edges ix vis) shown seeds
                                     {:height (* max-label-scale 30)
                                      :widths (into {} (for [id vis]
                                                         [id (label-width (get-in ix [:concepts id :concept/label]))]))})
                :from (into shown (for [[id s] seeds :when (shown s)] [id (shown s)]))
                :t    0.0)))))

(defn graph-reset
  "Restart the graph from `id`, expanded."
  [db id]
  (-> db
      (update :graph assoc :focus id :expanded #{id} :scope nil :pos {} :from {})
      ;; another graph: its view is automatic again
      (update-in [:graph :view] viewport/automatic-again)
      graph-relayout))

(defn graph-scope
  "Show exactly the concepts `ids` (a cluster), laid out afresh."
  [db ids]
  (if (= (set ids) (get-in db [:graph :scope]))
    db
    (-> db
        (update :graph assoc :scope (set ids) :focus nil :expanded #{} :pos {} :from {})
        (update-in [:graph :view] viewport/automatic-again)
        graph-relayout)))

(defn show-in-graph
  "Keep the graph if `id` is visible in it, else restart it from `id`."
  [db id]
  (if (contains? (graph-visible (:graph db)) id)
    db
    (graph-reset db id)))

(defn graph-toggle [db id]
  (-> db
      (update-in [:graph :expanded] #(if (contains? % id) (disj % id) (conj % id)))
      graph-relayout))

(defn graph-click
  "Clicking a concept in the graph opens it and expands it; clicking the
  open concept again collapses or re-expands it."
  [db id]
  (cond
    ;; a cluster's graph shows the cluster: open, never expand
    (get-in db [:graph :scope])                     (open-concept db id)
    (= id (:concept-id db))                         (graph-toggle db id)
    (contains? (get-in db [:graph :expanded]) id)   (open-concept db id)
    :else                                           (-> db (open-concept id) (graph-toggle id))))

(defn graph-tick [db t] (assoc-in db [:graph :t] t))

(defn graph-fetched [db data]
  (-> db
      (assoc-in [:graph :ix] (graph/index data))
      graph-relayout))

(defn graph-animating? [db] (< (get-in db [:graph :t]) 1))

(defn- radius [mentions] (+ 6 (* 2.2 (Math/sqrt (or mentions 0)))))

(defn graph-scene
  "What the graph view draws: {:nodes [..] :edges [..] :box [x y w h] :scale s}."
  [db]
  (let [{:keys [ix focus expanded] :as g} (:graph db)
        vis (graph-visible g)
        pos (select-keys (shown-positions g) vis)
        sel (:concept-id db)
        ;; the box covers labels too, not just dots
        box (layout/view-box (into {} (for [[id [x y]] pos
                                            :let [hw (/ (label-width (get-in ix [:concepts id :concept/label])) 2)]
                                            [k p] [[[id 0] [(- x hw) (- y 20)]] [[id 1] [(+ x hw) (+ y 34)]]]]
                                        [k p]))
                             {:pad 24})
        nodes (->> vis
                     (keep (fn [id]
                             (when-let [[x y] (pos id)]
                               (let [{:concept/keys [label gloss mentions]} (get-in ix [:concepts id])]
                                 {:id id :label label :gloss gloss :mentions mentions
                                  :score (item-score db (concept-item id))
                                  :x x :y y :r (radius mentions)
                                  :focus? (= id focus) :selected? (= id sel)
                                  :expanded? (contains? expanded id)
                                  :hidden (if (:scope g) 0 (graph/hidden-count ix vis id))}))))
                     ;; the selected concept is drawn last, on top
                     (sort-by (juxt :selected? :label))
                     vec)
          edges (vec (for [{:keys [from to] :as e} (graph/edges ix vis)
                           :let [[x1 y1] (pos from), [x2 y2] (pos to)]
                           :when (and x1 x2)]
                       (assoc e :x1 x1 :y1 y1 :x2 x2 :y2 y2
                              :score (item-score db (relation-item from to)))))]
      {:nodes nodes
       :edges edges
       :box   box
       :ideas (graph-ideas db nodes edges)
       ;; labels grow with the zoom-out, so they stay readable (up to a point)
       :scale (-> (/ (nth box 2) 640) (max 1) (min max-label-scale))}))

;;=============================================================================
;; The map: every cluster (topic) of the knowledge
;;=============================================================================

(defn bubble-radius
  "A cluster's bubble grows with its concepts (by area), in map units."
  [n]
  (+ 16 (* 4.5 (Math/sqrt n))))

(def map-font
  "Topic names on the map, in map units (the map is drawn at a fixed size)."
  22)

(defn label-lines
  "A topic name on at most two lines, split at the space nearest the middle."
  [label]
  (let [words (str/split (str label) #" ")]
    (if (or (< (count label) 17) (< (count words) 2))
      [label]
      (let [half  (/ (count label) 2)
            cuts  (reductions + (map #(inc (count %)) words))
            k     (inc (apply min-key #(abs (- (nth cuts %) half)) (range (dec (count words)))))]
        [(str/join " " (take k words)) (str/join " " (drop k words))]))))

^:rct/test
(comment
  (label-lines "Backward induction") ;=> ["Backward" "induction"]
  (label-lines "Bayesian equilibria and Harsanyi transformation") ;=> ["Bayesian equilibria and" "Harsanyi transformation"]
  (label-lines "Auctions") ;=> ["Auctions"]
  )

(defn- name-width [label] (+ 16 (* 0.56 map-font (apply max (map count (label-lines label))))))

(def ^:private map-links-per-cluster
  "Each topic is drawn joined to its few strongest neighbours: enough to see
  what sits near what, not a web."
  3)

(def ^:private map-link-share
  "A link is drawn only if it is at least this share of the strongest link of
  each end: a few shared words (a homonym, a passing mention) join two
  fields as strongly, in absolute terms, as two neighbouring topics are
  joined, but never strongly next to what each field holds together."
  0.25)

(defn strongest-links
  "Links among `ids`, keeping for each cluster its `k` strongest, and of
  those only the ones not weak next to either end's strongest."
  [links ids k]
  (let [in   (filter #(and (contains? ids (:from %)) (contains? ids (:to %))) links)
        of   (fn [id] (filter #(or (= id (:from %)) (= id (:to %))) in))
        best (memoize (fn [id] (reduce max 0 (map :weight (of id)))))
        top  (fn [id] (->> (of id)
                           (sort-by (juxt (comp - :weight) (comp str :from) (comp str :to)))
                           (take k)))]
    (->> (mapcat top (sort-by str ids))
         distinct
         (filter (fn [{:keys [from to weight]}]
                   (>= weight (* map-link-share (min (best from) (best to))))))
         vec)))

^:rct/test
(comment
  ;; two fields, each held together, joined by a few shared words
  (strongest-links [{:from :a :to :b :weight 10.0} {:from :x :to :y :weight 8.0}
                    {:from :b :to :x :weight 1.5} {:from :a :to :leaf :weight 0.8}]
                   #{:a :b :x :y :leaf} 3)
  ;=> [{:from :a :to :b :weight 10.0} {:from :a :to :leaf :weight 0.8} {:from :x :to :y :weight 8.0}]
  )

(defn- map-layout
  "Positions {cluster-id [x y]}: topics placed by their links alone (each
  joined to its strongest few), whatever sources their passages come from:
  the knowledge is what is mapped, so topics that share edges sit close and
  only unconnected knowledge stands apart (see layout/layout). Each topic's
  box is its bubble with its name below, so none overlap."
  [{:keys [clusters links]}]
  (let [ids (set (map :cluster/id clusters))]
    (when (seq ids)
      (layout/layout ids
                     (map #(assoc % :kind :related) (strongest-links links ids map-links-per-cluster))
                     {} {}
                     {:length 150 :iterations 300 :aspect 1.0
                      ;; one box height for all (the layout's): a typical topic's, not the largest
                      :height (let [rs (sort (map (comp bubble-radius count :concepts) clusters))]
                                (+ (* 2 (nth rs (quot (count rs) 2))) (* 2.4 map-font)))
                      :widths (into {} (for [{:cluster/keys [id label] :keys [concepts]} clusters]
                                         [id (max (* 2 (bubble-radius (count concepts))) (name-width label))]))}))))

(defn cluster-of
  "Cluster `id` from the universe, or nil."
  [db id]
  (some #(when (= id (:cluster/id %)) %) (get-in db [:universe :clusters])))

(defn concept-cluster
  "The cluster concept `id` belongs to, or nil."
  [db id]
  (some #(when (some #{id} (:concepts %)) %) (get-in db [:universe :clusters])))

(defn current-cluster [db] (cluster-of db (:cluster-id db)))

(defn universe-fetched [db u]
  (let [db (assoc db :universe (assoc u :pos (map-layout u)) :loading? false)]
    (if-let [c (and (= :cluster (:view db)) (current-cluster db))]
      (graph-scope db (:concepts c))
      db)))

(defn cluster-fetched [db id detail] (assoc-in db [:clusters id] detail))

(defn learning-clusters
  "The clusters being learned, in the order chosen."
  [db]
  (keep #(cluster-of db %) (get-in db [:universe :goals])))

(defn learning-toggled
  "Start (`on?`) or stop learning cluster `id`, shown at once. What to study
  next changes with it, so the learning overview is fetched again."
  [db id on?]
  (-> db
      (update-in [:universe :goals] (fn [gs] (if on? (vec (distinct (conj (vec gs) id))) (vec (remove #{id} gs)))))
      (update-in [:universe :clusters] (fn [cs] (mapv #(cond-> % (= id (:cluster/id %)) (assoc :learning? on?)) cs)))
      (assoc :learning nil)))

(defn map-scene
  "What the map draws: {:bubbles [{:id :label :lines :gloss :n :score
  :learning? :x :y :r}] :links [{:x1 :y1 :x2 :y2 :weight}] :box [x y w h]},
  in map units (fixed sizes, see map-font)."
  [db]
  (let [{:keys [clusters links pos]} (:universe db)
        bubbles (vec (for [{:cluster/keys [id label gloss] :keys [concepts score learning?]} clusters
                           :let [[x y] (pos id)] :when x]
                       {:id id :label label :lines (label-lines label) :gloss gloss :n (count concepts)
                        :score (or score 0) :learning? learning? :x x :y y
                        :r (bubble-radius (count concepts))}))
        at      (into {} (map (juxt :id identity)) bubbles)]
    {:bubbles bubbles
     :links   (vec (for [{:keys [from to weight]} (strongest-links links (set (keys at)) map-links-per-cluster)
                         :let [a (at from) b (at to)]]
                     {:x1 (:x a) :y1 (:y a) :x2 (:x b) :y2 (:y b) :weight weight}))
     :box     (layout/view-box (into {} (for [{:keys [id x y r label lines]} bubbles
                                              :let [hw (max r (/ (name-width label) 2))]
                                              [k p] [[[id 0] [(- x hw) (- y r)]]
                                                     [[id 1] [(+ x hw) (+ y r (* 1.3 map-font (inc (count lines))))]]]]
                                          [k p]))
                               {:pad 20})}))

(def ^:private graph-font
  "Concept names in the graph at label scale 1, in graph units."
  12)

(defn- pane
  "A pannable, zoomable pane (`:map` or `:graph`): where its view state is,
  what it draws (its box) and the viewport options (see viewport)."
  [db k]
  (case k
    :map   (let [{:keys [bubbles box]} (map-scene db)
                 learning (for [b bubbles :when (:learning? b)] [(:x b) (:y b)])]
             {:path [:map-view] :box (when (seq bubbles) box)
              :opts {:font map-font :max-auto 1.2
                     :focus (when (seq learning)
                              [(/ (reduce + (map first learning)) (count learning))
                               (/ (reduce + (map second learning)) (count learning))])}})
    :graph (let [{:keys [nodes box scale]} (graph-scene db)
                 at (some #(when (:selected? %) [(:x %) (:y %)]) nodes)]
             {:path [:graph :view] :box (when (seq nodes) box)
              :opts {:font (* graph-font scale) :max-auto 2.0 :focus at}})))

(defn pane-view
  "What pane `k` shows now: {:view {:scale :center} (nil before its size is
  known) :view-box [x y w h] :labels? bool}."
  [db k]
  (let [{:keys [path box opts]} (pane db k)
        state (get-in db path)
        v     (viewport/current state box opts)]
    {:view     v
     :view-box (if v (viewport/view-box v (:size state)) box)
     :labels?  (viewport/labels? v (:font opts))}))

(defn pane-sized [db k w h]
  (let [{:keys [path]} (pane db k)]
    (if (= [w h] (get-in db (conj path :size)))
      db
      (update-in db path viewport/sized w h))))

(defn pane-panned [db k dx dy]
  (let [{:keys [path box opts]} (pane db k)]
    (update-in db path viewport/panned box opts dx dy)))

(defn pane-zoomed
  ([db k factor] (pane-zoomed db k factor nil))
  ([db k factor at]
   (let [{:keys [path box opts]} (pane db k)]
     (update-in db path viewport/zoomed box opts factor at))))

(defn pane-whole [db k]
  (let [{:keys [path box]} (pane db k)]
    (update-in db path viewport/whole box)))

^:rct/test
(comment
  ;; the map follows its view state; before the pane's size is known, the whole box
  (def u {:clusters [{:cluster/id :a :cluster/label "A" :concepts [1 2]} {:cluster/id :b :cluster/label "B" :concepts [3]}]
          :links [{:from :a :to :b :weight 1.0}]})
  (def m (universe-fetched initial-state u))
  (= (:view-box (pane-view m :map)) (:box (map-scene m))) ;=> true
  (def sized (pane-sized m :map 800 600))
  (:labels? (pane-view sized :map)) ;=> true
  ;; zoomed and panned: manual from then on
  (some? (get-in (pane-zoomed sized :map 2) [:map-view :manual])) ;=> true
  (get-in (pane-whole sized :map) [:map-view :size]) ;=> [800 600]
  )

(defn keyword-fetched [db kw nodes]
  (-> db
      (assoc-in [:keyword-results kw] (vec nodes))
      (assoc :loading? false)))

;;=============================================================================
;; Source pane: the source document's pages (a PDF's, an EPUB's)
;;=============================================================================

(defn node-page
  "Page a node points at in its source: its own page, else its first child's."
  [node]
  (or (:node/page node)
      (some :node/page (:node/children node))
      1))

(defn doc-page
  "Page shown in the source pane."
  [db]
  (or (get-in db [:doc :page])
      (some-> (current-node db) node-page)
      1))

(defn doc-pages [db source-id] (get-in db [:doc :pages source-id]))

(defn doc-loaded
  "Source `source-id` has `n` pages; `aspect` (width/height of one) sizes
  the pages not drawn yet."
  ([db source-id n] (doc-loaded db source-id n nil))
  ([db source-id n aspect]
   (cond-> (assoc-in db [:doc :pages source-id] n)
     (and aspect (not (get-in db [:doc :aspect source-id])))
     (assoc-in [:doc :aspect source-id] aspect))))

(defn doc-aspect [db source-id] (get-in db [:doc :aspect source-id]))

(defn doc-html-loaded
  "The pages of source `source-id` at `url` came, as HTML: [html ..]."
  [db source-id url pages]
  (-> db
      (assoc-in [:doc :html url] (vec pages))
      (assoc-in [:doc :pages source-id] (count pages))))

(defn doc-html
  "The HTML of page `p` at `url`, once loaded."
  [db url p]
  (get-in db [:doc :html url (dec p)]))

(def doc-zooms
  "Zoom steps of the source's pages (1: the pane's width)."
  [0.5 0.67 0.8 1.0 1.25 1.5 2.0 2.5 3.0])

(defn doc-zoom [db] (get-in db [:doc :zoom] 1.0))

(defn card-open?
  "Whether the summary of the card of node `id` is read in full."
  [db id]
  (let [{:keys [all? open]} (:cards db)]
    (if all? (not (contains? open id)) (contains? open id))))

(defn card-toggled
  "Read the summary of node `id`'s card in full, or fold it again."
  [db id]
  (update-in db [:cards :open] (fn [o] (let [o (or o #{})] (if (contains? o id) (disj o id) (conj o id))))))

(defn cards-all
  "Every drill-down summary in full (`all?`), or folded; cards opened or
  folded one by one follow."
  [db all?]
  (assoc db :cards {:all? (boolean all?) :open #{}}))

(def split-range "The left pane's share of the width: from, to." [0.25 0.75])

(defn split-share
  "The left pane's share of the width now (while dragged, where the divider is)."
  [db]
  (or (:split-drag db) (:split db) 0.5))

(defn split-dragged
  "The divider is being dragged to `f` of the width (kept within range)."
  [db f]
  (let [[lo hi] split-range] (assoc db :split-drag (-> f (max lo) (min hi)))))

(defn split-set
  "The left pane takes `f` of the width (nil: where it was dragged to)."
  [db f]
  (let [[lo hi] split-range
        f (or f (:split-drag db) (:split db) 0.5)]
    (assoc db :split (-> (/ (Math/round (* 1000.0 f)) 1000.0) (max lo) (min hi)) :split-drag nil)))

(defn doc-zoomed
  "Pages at `z` x the pane's width (rounded, within the steps' range)."
  [db z]
  (assoc-in db [:doc :zoom] (-> (/ (Math/round (* 100.0 z)) 100.0)
                                (max (first doc-zooms)) (min (peek doc-zooms)))))

(defn doc-zoom-step
  "One step in (`dir` 1) or out (-1) from the zoom shown."
  [db dir]
  (let [z (doc-zoom db)
        to (if (pos? dir)
             (or (first (filter #(> % (+ z 0.001)) doc-zooms)) (peek doc-zooms))
             (or (last (filter #(< % (- z 0.001)) doc-zooms)) (first doc-zooms)))]
    (doc-zoomed db to)))

(defn doc-scrolled
  "The learner scrolled the source to page `p`: it no longer follows the node."
  [db p]
  (assoc-in db [:doc :page] p))

(declare wide?)

(defn page-sync
  "{:source/id :page :near} to ask which node goes beside the page the source
  was scrolled to (the left pane follows the source), or nil: not on a node
  page, the left pane hidden, or the source following the node."
  [db]
  (let [node (current-node db) p (get-in db [:doc :page])]
    (when (and (= :node (:view db)) node p (not (wide? db)))
      {:source/id (get-in node [:node/source :source/id]) :page p :near (:node/id node)})))

(defn page-synced
  "The node `id` goes beside page `p` of the source: show it, the source
  staying where the learner scrolled it. Nothing when it is the node shown,
  or the learner has scrolled on or gone elsewhere meanwhile."
  [db p id]
  (if (and id (= :node (:view db)) (not= id (:node-id db)) (= p (get-in db [:doc :page])))
    (-> db (open-node id) (assoc-in [:doc :page] p))
    db))

(defn doc-window
  "Whether page `q` is near page `p` (the one at the top of the pane), so
  worth drawing: the others are blank sheets of the right size. Fewer when
  zoomed in (pages are big, and costly to keep drawn)."
  ([p q] (doc-window p q 1.0))
  ([p q zoom]
   (if (> zoom 1.5)
     (<= (- p 1) q (+ p 1))
     (<= (- p 2) q (+ p 3)))))

(defn doc-goto
  "Show page `p` (clamped to the document) regardless of the selected node."
  [db source-id p]
  (let [n (or (doc-pages db source-id) p)]
    (assoc-in db [:doc :page] (-> p (max 1) (min n)))))

(defn doc-page-typed
  "The learner is typing `s` as the page to go to."
  [db s]
  (assoc-in db [:doc :draft] s))

(defn doc-page-entered
  "Go to the page typed (clamped to the document), if it is a number; the
  typing ends either way."
  [db source-id]
  (let [p (some->> (get-in db [:doc :draft]) (re-find #"\d+") parse-long)]
    (cond-> (assoc-in db [:doc :draft] nil)
      p (doc-goto source-id p))))

(defn doc-follow [db] (assoc-in db [:doc :page] nil))

(defn following? [db] (nil? (get-in db [:doc :page])))

(defn source-only
  "Hide the left pane of split views (the source gets the whole width), or show it."
  [db on?]
  (assoc db :source-only? (boolean on?)))

(defn source-only-toggle [db] (source-only db (not (:source-only? db))))

(defn fullscreen
  "The browser went full screen (`on?`) or back: only the pane is shown."
  [db on?]
  (assoc db :fullscreen? (boolean on?)))

(defn wide?
  "Whether the source pane has the whole width (left pane hidden, or full screen)."
  [db]
  (boolean (or (:source-only? db) (:fullscreen? db))))

(defn highlight-texts
  "Source text of `node` to mark on its source page: a paragraph's text, a
  passage's paragraphs, a section's heading."
  [{:node/keys [kind text title children]}]
  (case kind
    :paragraph (when text [text])
    :group     (vec (keep :node/text children))
    :section   (when title [title])
    nil))

(defn highlight-key
  "Identifies the highlight wanted now: document, the node's page and node
  (wherever the source is scrolled to)."
  [db url]
  [url (some-> (current-node db) node-page) (:node-id db)])

(defn highlight
  "The highlight for `key` once computed (rects as fractions of the page)."
  [db key]
  (let [h (get-in db [:doc :highlight])]
    (when (= key (:key h)) h)))

(defn highlight-found [db key result]
  (assoc-in db [:doc :highlight] (assoc result :key key)))

(defn doc-quotes
  "Ideas on text selected on page `p` of source `sid`:
  [{:idea .. :anchor quote-anchor}]."
  [db sid p]
  (vec (for [i (get-in db [:ideas :all])
             a (:idea/anchors i)
             :when (and (= :quote (:anchor/kind a)) (= :page (:field a))
                        (= sid (:source/id a)) (= p (:page a)))]
         {:idea i :anchor a})))

(defn keypoints-fetched [db sid kps] (assoc-in db [:keypoints sid] (vec kps)))

(defn page-keypoints
  "Key sentences that may be on page `p` of source `sid`: their paragraph
  starts there or on the page before (it may run over)."
  [db sid p]
  (filterv #(<= (dec p) (:page %) p) (get-in db [:keypoints sid])))

(defn keypoints-placed
  "Where the key sentences `kps` are on page `p` ({:rects [rects-or-nil ..]
  :left margin :aspect w/h}, rects in order): kept by page, with the
  sentences they are of."
  [db url p kps {:keys [rects left aspect]}]
  (assoc-in db [:doc :key-rects (str url "#" p)] {:kps kps :rects rects :left left :aspect aspect}))

(defn keypoint-rects
  "{:found [[keypoint rects] ..] :left margin :aspect w/h} on page `p` of
  `url`, once placed for these `kps`; nil while not."
  [db url p kps]
  (let [{placed-kps :kps :keys [rects left aspect]} (get-in db [:doc :key-rects (str url "#" p)])]
    (when (= placed-kps kps)
      {:found (vec (keep (fn [[k r]] (when r [k r])) (map vector kps rects))) :left left :aspect aspect})))

(defn keypoint-at
  "Where key sentence `kp` ({:para :quote}) was found: [page y] (y a fraction
  of the page), on its paragraph's page or the next (it may run over); nil
  while not placed."
  [db url sid page kp]
  (some (fn [q]
          (some (fn [[k rects]] (when (= (:quote k) (:quote kp)) [q (:y (first rects))]))
                (:found (keypoint-rects db url q (page-keypoints db sid q)))))
        [page (inc page)]))

(defn doc-goto-target
  "Show page `p` with `y` (a fraction of it, or nil: its top) at the top of
  the pane: a key sentence clicked on the left."
  [db source-id p y]
  (-> (doc-goto db source-id p)
      (assoc-in [:doc :target] {:page p :y y :n (inc (get-in db [:doc :target :n] 0))})))

(defn doc-target
  "{:y :key} to scroll page `p` to, when a key sentence there was clicked."
  [db p]
  (let [{:keys [page y n]} (get-in db [:doc :target])]
    (when (and y (= page p)) {:y y :key (str "target-" n)})))

(defn open-texts
  "Text of `node` that may be on page `q`, to mark it open there: a
  passage's paragraphs starting on `q`, and the last one starting on the
  page before (it may run over); a paragraph on its page and the next; a
  section's heading on its page."
  [{:node/keys [kind text title page children]} q]
  (case kind
    :paragraph (when (and text page (<= page q (inc page))) [text])
    :section   (when (and title (= page q)) [title])
    :group     (let [paras (filter :node/text children)
                     on    (filter #(= q (:node/page %)) paras)
                     before (last (filter #(= (dec q) (:node/page %)) paras))]
                 (not-empty (mapv :node/text (cond->> on before (cons before)))))
    nil))

(defn open-placed
  "Where the texts of node `id` open on page `p` of `url` are: a bar spans
  from the first to the last rect."
  [db url p id texts rects]
  (assoc-in db [:doc :open-rects (str url "#" p)] {:id id :texts texts :rects (vec (mapcat identity rects))}))

(defn open-span
  "[top bottom left] of node `id`'s `texts` on page `p` (fractions), once
  placed; :pending while not."
  [db url p id texts]
  (let [{pid :id ptexts :texts rects :rects} (get-in db [:doc :open-rects (str url "#" p)])]
    (if (and (= pid id) (= ptexts texts))
      (when (seq rects)
        [(apply min (map :y rects)) (apply max (map #(+ (:y %) (:h %)) rects)) (apply min (map :x rects))])
      :pending)))

(defn doc-quotes-key
  "Identifies where `quotes` are on page `p` of `sid`: boxes are fractions of
  the page, the same at any width."
  [sid p quotes]
  [sid p (vec (sort (map (comp str :idea/id :idea) quotes)))])

(defn text-layer-ready [db url p] (update-in db [:doc :text-ready] (fnil conj #{}) (str url "#" p)))

(defn text-layer-ready? [db url p] (contains? (get-in db [:doc :text-ready]) (str url "#" p)))

(defn doc-quotes-placed
  "Boxes of the quotes keyed `k` were found: {idea-id [{:x :y :w :h}]}."
  [db k placed]
  (assoc-in db [:doc :placed k] placed))

(defn doc-quotes-placed? [db k] (contains? (get-in db [:doc :placed]) k))

^:rct/test
(comment
  (def id #uuid "00000000-0000-0000-0000-000000000001")
  (def n {:node/id id :node/page 7 :node/children [{:node/page 8}]
          :node/source {:source/id :s}})

  ;; opening an uncached node shows loading; cached does not
  (:loading? (open-node initial-state id)) ;=> true
  (:loading? (-> initial-state (node-fetched n) (open-node id))) ;=> false

  ;; the pane follows the node's page, a section falls back to its first child's
  (doc-page (-> initial-state (open-node id) (node-fetched n))) ;=> 7
  ;; a note on text selected on page 28 of the source is drawn on that page
  (def pq {:anchor/kind :quote :field :page :source/id :s :page 28 :quote "Bool"})
  (def with-pq (ideas-fetched initial-state [{:idea/id :q1 :idea/stance :note :idea/anchors [pq]}
                                             {:idea/id :q2 :idea/stance :agree
                                              :idea/anchors [(assoc pq :field :text)]}]))
  (map (comp :idea/id :idea) (doc-quotes with-pq :s 28)) ;=> [:q1]
  (doc-quotes with-pq :s 29) ;=> []
  (doc-quotes-key :s 28 (doc-quotes with-pq :s 28)) ;=> [:s 28 [":q1"]]
  (text-layer-ready? (text-layer-ready initial-state "/f" 28) "/f" 28) ;=> true
  (doc-quotes-placed? (doc-quotes-placed initial-state [:k] {}) [:k]) ;=> true
  ;; what is open, page by page: a passage's paragraphs and the one running over
  (def g {:node/kind :group :node/page 4 :node/children [{:node/text "a" :node/page 4} {:node/text "b" :node/page 4}
                                                         {:node/text "c" :node/page 5}]})
  [(open-texts g 4) (open-texts g 5) (open-texts g 6) (open-texts g 7) (open-texts g 3)]
  ;=> [["a" "b"] ["b" "c"] ["c"] nil nil]
  (open-span (open-placed initial-state "/f" 5 :g ["b" "c"] [[{:x 0.1 :y 0.2 :w 0.5 :h 0.02}] [{:x 0.12 :y 0.4 :w 0.5 :h 0.02}]])
             "/f" 5 :g ["b" "c"]) ;=> [0.2 0.42000000000000004 0.1]
  (open-span initial-state "/f" 5 :g ["b"]) ;=> :pending
  (doc-target (doc-goto-target (doc-loaded initial-state :s 9) :s 5 0.3) 5) ;=> {:y 0.3 :key "target-1"}
  ;; key sentences: those of the page and the one before, placed once per set
  (def kd (keypoints-fetched initial-state :s [{:page 4 :quote "a"} {:page 5 :quote "b"} {:page 7 :quote "c"}]))
  (map :quote (page-keypoints kd :s 5)) ;=> ["a" "b"]
  (keypoint-rects kd "/f" 5 (page-keypoints kd :s 5)) ;=> nil
  (map (comp :quote first) (:found (keypoint-rects (keypoints-placed kd "/f" 5 (page-keypoints kd :s 5) {:rects [nil [{:x 0}]] :left 0.1})
                                                   "/f" 5 (page-keypoints kd :s 5)))) ;=> ["b"]
  (:source-only? (source-only-toggle initial-state)) ;=> true
  (:source-only? (-> initial-state source-only-toggle source-only-toggle)) ;=> false
  (:source-only? (source-only initial-state "yes")) ;=> true
  ;; typing a page number in the toolbar
  (let [d (-> initial-state (doc-loaded :s 120) (doc-page-typed "88"))]
    [(get-in d [:doc :draft]) (get-in (doc-page-entered d :s) [:doc :page])]) ;=> ["88" 88]
  (get-in (-> initial-state (doc-loaded :s 120) (doc-page-typed "999") (doc-page-entered :s)) [:doc :page]) ;=> 120
  (get-in (-> initial-state (doc-page-typed "abc") (doc-page-entered :s)) [:doc]) ;=> (:doc initial-state)
  ;; the left pane follows the source scrolled to another page
  (let [a #uuid "00000000-0000-0000-0000-00000000000a" b #uuid "00000000-0000-0000-0000-00000000000b"
        d (-> initial-state (open-node a)
              (assoc-in [:nodes a] {:node/id a :node/source {:source/id :s} :node/page 3})
              (doc-scrolled 9))]
    [(page-sync d) (page-sync (doc-follow d)) (page-sync (source-only d true))
     (let [d2 (page-synced d 9 b)] [(:node-id d2) (doc-page d2)])
     (:node-id (page-synced (doc-scrolled d 10) 9 b))])
  ;=> [{:source/id :s :page 9 :near #uuid "00000000-0000-0000-0000-00000000000a"} nil nil
  ;    [#uuid "00000000-0000-0000-0000-00000000000b" 9] #uuid "00000000-0000-0000-0000-00000000000a"]
  ;; bookmarks: newest first, at the page they were made on; not among the ideas of a node
  (let [bm (fn [id node at page] {:idea/id id :idea/stance :bookmark :idea/at at
                                  :idea/anchors [{:anchor/kind :node :node/id node :page page}]
                                  :idea/targets [{:key [:node node] :kind :node :title (name node) :page 1
                                                  :source/id :s :source/title "Book"}]})
        d  (ideas-fetched initial-state [(bm :b1 :n1 1 12) (bm :b2 :n2 2 nil)])]
    [(mapv (juxt :node-id :page) (bookmarks d)) (:idea/id (bookmark-on d :n1)) (ideas-in-node d :n1 nil)
     (let [o (open-bookmark d (first (bookmarks d :s)))] [(:node-id o) (doc-page o)])])
  ;=> [[[:n2 1] [:n1 12]] :b1 [] [:n2 1]]
  ;; drill-down cards: one by one, or all (then a toggle folds one)
  [(card-open? (card-toggled initial-state :a) :a) (card-open? (cards-all initial-state true) :a)
   (card-open? (-> initial-state (cards-all true) (card-toggled :a)) :a)] ;=> [true true false]
  ;; the divider between the panes: live while dragged, kept on release, within range
  (split-share (split-dragged initial-state 0.9)) ;=> 0.75
  ((juxt :split :split-drag) (split-set (split-dragged initial-state 0.6234) nil)) ;=> [0.623 nil]
  (:split (split-set initial-state 0.1)) ;=> 0.25
  (wide? (fullscreen initial-state true)) ;=> true
  (wide? initial-state) ;=> false
  (node-page {:node/children [{:node/page 3}]}) ;=> 3
  (node-page {}) ;=> 1

  ;; manual paging is clamped and survives until the next node is opened
  (def shown-n (-> initial-state (open-node id) (node-fetched n)))
  (def paged (-> initial-state (open-node id) (node-fetched n) (doc-loaded :s 10) (doc-goto :s 42)))
  (doc-page paged) ;=> 10
  (following? paged) ;=> false
  (doc-page (open-node paged id)) ;=> 7
  ;; scrolling the source turns its page; the first page drawn sizes the rest
  (doc-page (doc-scrolled shown-n 9)) ;=> 9
  (following? (doc-scrolled shown-n 9)) ;=> false
  (doc-aspect (-> initial-state (doc-loaded :s 10 0.7) (doc-loaded :s 10 0.8)) :s) ;=> 0.7
  (filter #(doc-window 5 %) (range 1 12)) ;=> [3 4 5 6 7 8]
  (filter #(doc-window 5 % 2.0) (range 1 12)) ;=> [4 5 6]
  ;; zoom: steps in and out, kept within range
  (doc-zoom (doc-zoom-step initial-state 1)) ;=> 1.25
  (doc-zoom (doc-zoom-step (doc-zoomed initial-state 1.1) -1)) ;=> 1.0
  (doc-zoom (doc-zoom-step (doc-zoomed initial-state 3.0) 1)) ;=> 3.0
  (doc-zoom (doc-zoomed initial-state 9)) ;=> 3.0

  ;; what gets highlighted
  (highlight-texts {:node/kind :paragraph :node/text "Some text."}) ;=> ["Some text."]
  (highlight-texts {:node/kind :group :node/children [{:node/text "a"} {:node/text "b"}]}) ;=> ["a" "b"]
  (highlight-texts {:node/kind :section :node/title "3.3 Normalization"}) ;=> ["3.3 Normalization"]
  (highlight-texts {:node/kind :source}) ;=> nil

  ;; a highlight is kept for its page and node only
  (def shown (-> initial-state (open-node id) (node-fetched n)))
  (def k (highlight-key shown "/f"))
  k ;=> ["/f" 7 #uuid "00000000-0000-0000-0000-000000000001"]
  (:aspect (highlight (highlight-found shown k {:aspect 0.7 :rects []}) k)) ;=> 0.7
  (highlight (highlight-found shown k {:aspect 0.7 :rects []}) ["/f" 8 id]) ;=> nil
  ;; the node's page, wherever the source is scrolled to
  (highlight-key (doc-scrolled shown 12) "/f") ;=> ["/f" 7 #uuid "00000000-0000-0000-0000-000000000001"]

  ;; missing node -> error
  (:code (:error (node-fetched initial-state nil))) ;=> :not-found

  ;; asking: the answer is shown only if the user still waits for it
  (def a {:ask/id id :ask/question "Why?" :ask/title "Because" :ask/answer "..."})
  (def asked (ask-submitted initial-state "Why?"))
  (:asking asked) ;=> "Why?"
  (:ask-id (ask-created asked "Why?" a)) ;=> #uuid "00000000-0000-0000-0000-000000000001"
  (:ask-id (ask-created (go-home asked) "Why?" a)) ;=> nil
  (map :ask/title (:ask-history (ask-created (go-home asked) "Why?" a))) ;=> ["Because"]
  (:asking (set-error asked "boom")) ;=> nil

  ;; concepts
  (:loading? (open-concept initial-state id)) ;=> true
  (:concept/label (current-concept (-> initial-state (open-concept id)
                                       (concept-fetched {:concept/id id :concept/label "confluence"}))))
  ;=> "confluence"
  (:code (:error (concept-fetched initial-state nil))) ;=> :not-found
  (def idx (concept-index-fetched initial-state
                                  [{:concept/label "confluence" :concept/gloss "Divergent reductions rejoin."}
                                   {:concept/label "parallel reduction" :concept/gloss "Contracts many redexes."}]))
  (map :concept/label (filtered-concepts (set-concept-filter idx "REDUC"))) ;=> ["confluence" "parallel reduction"]
  (map :concept/label (filtered-concepts (set-concept-filter idx "parallel red"))) ;=> ["parallel reduction"]
  (count (filtered-concepts idx)) ;=> 2

  ;; concept graph: :t is a kind of :p and relates to :a; :a relates to :d
  (def gdata {:concepts [{:concept/id :p :concept/label "p" :concept/mentions 4}
                         {:concept/id :t :concept/label "t" :concept/mentions 9}
                         {:concept/id :a :concept/label "a"} {:concept/id :d :concept/label "d"}]
              :edges [{:from :t :to :p :kind :broader}
                      {:from :a :to :t :kind :related :weight 0.5 :shared 3}
                      {:from :a :to :d :kind :related :weight 0.4 :shared 2}]})
  ;; the graph may arrive before or after the concept is opened
  (def g1 (-> initial-state (open-concept :t) (graph-fetched gdata)))
  (= g1 (-> initial-state (graph-fetched gdata) (open-concept :t))) ;=> true
  (set (map :id (:nodes (graph-scene g1)))) ;=> #{:t :p :a}
  (map (juxt :id :hidden :expanded? :selected?) (sort-by :label (:nodes (graph-scene g1))))
  ;=> [[:a 1 false false] [:p 0 false false] [:t 0 true true]]
  (count (:edges (graph-scene g1))) ;=> 2

  ;; clicking :a opens and expands it; the graph keeps its shape
  (def g2 (graph-click g1 :a))
  [(:concept-id g2) (get-in g2 [:graph :focus])] ;=> [:a :t]
  (set (map :id (:nodes (graph-scene g2)))) ;=> #{:t :p :a :d}
  ;; :d starts the animation at :a, where it grows from
  (= (get-in g2 [:graph :from :d]) (get-in g1 [:graph :pos :a])) ;=> true
  (graph-animating? g2) ;=> true
  ;; clicking the open concept again collapses it
  (set (map :id (:nodes (graph-scene (graph-click (graph-tick g2 1.0) :a))))) ;=> #{:t :p :a}
  ;; collapsing the focus hides everything else
  (count (:nodes (graph-scene (graph-toggle (open-concept g1 :t) :t)))) ;=> 1
  ;; opening a concept not in the graph restarts it there
  (get-in (open-concept g1 :d) [:graph :focus]) ;=> :d

  ;; scores reach the graph: concepts and relations, unknown = 0
  (def scored (learning-fetched g1 {:scores {[:concept :t] 0.4 (relation-item :t :p) 0.2}}))
  (:score (first (filter #(= :t (:id %)) (:nodes (graph-scene scored))))) ;=> 0.4
  (set (map :score (:edges (graph-scene scored)))) ;=> #{0.2 0}

  ;; the top box suggests what is typed names; until its own answer comes, a prefix's
  (def typed (-> initial-state (box-typed "nas") (goto-fetched "nas" {:concepts [{:concept/label "nash"}]})))
  (goto-query typed) ;=> "nas"
  (:concepts (box-suggestions (box-typed typed "nash "))) ;=> [{:concept/label "nash"}]
  (box-suggestions (box-open typed false)) ;=> nil
  (goto-query (box-typed typed "n")) ;=> nil

  ;; a quiz: ask, answer, grade; new scores come with the grading
  (def asked (quiz-start initial-state [:concept :t]))
  ;; asked where the learner is: no page change
  [(:view asked) (get-in asked [:quiz :status])] ;=> [:home :asking]
  (get-in (quiz-fold asked true) [:quiz :folded?]) ;=> true
  (def answering (question-created asked {:question/id 1 :question/item [:concept :t] :question/prompt "Why?"}))
  (get-in answering [:quiz :status]) ;=> :answering
  (def graded (-> answering (quiz-typed "Because.") quiz-submitted
                  (quiz-graded {:grade 0.5 :overview {:overall 0.1 :scores {[:concept :t] 0.25}}})))
  [(get-in graded [:quiz :status]) (get-in graded [:quiz :response]) (item-score graded [:concept :t])]
  ;=> [:graded "Because." 0.25]
  ;; a failed model call goes back a step, so the learner can retry
  (get-in (set-error (quiz-submitted answering) "boom") [:quiz :status]) ;=> :answering
  (get-in (set-error asked "boom") [:quiz :status]) ;=> nil
  ;; a late question for a quiz already closed is dropped
  (get-in (question-created (quiz-close asked) {:question/id 1}) [:quiz :status]) ;=> nil

  ;; ideas: grouped by what they are about; a note links to others
  (def ag {:idea/id :i1 :idea/stance :agree :idea/anchors [(concept-anchor :t)]
           :idea/targets [{:key [:concept :t]}]})
  (def nt {:idea/id :i2 :idea/stance :note :idea/text "Hm" :idea/links [:i1]
           :idea/anchors [{:anchor/kind :quote :field :text :quote "why"}]
           :idea/targets [{:key [:node :n]}]})
  (def withi (ideas-fetched g1 [ag nt]))
  (map :idea/id (ideas-on withi #{[:concept :t]})) ;=> [:i1]
  ;; a section shows ideas inside it, and on the page beside it
  (def inside {:idea/id :i5 :idea/targets [{:key [:node :para]}] :idea/within [:sec :chap]})
  (def on-page {:idea/id :i6 :idea/targets [{:key [:page :s 28]}]})
  (map :idea/id (ideas-in-node (ideas-fetched g1 [inside on-page ag]) :chap [:page :s 28])) ;=> [:i5 :i6]
  (map :idea/id (ideas-in-node (ideas-fetched g1 [inside on-page ag]) :other [:page :s 3])) ;=> []
  ;; text selected on the source page shown, found in another paragraph: listed (and marked) there
  (def from-page {:idea/id :i7 :idea/targets [{:key [:node :elsewhere]}]
                 :idea/anchors [{:anchor/kind :quote :field :page :source/id :s :page 3 :quote "x"}]})
  (map :idea/id (ideas-in-node (ideas-fetched g1 [from-page]) :other [:page :s 3])) ;=> [:i7]
  (= ag (reaction withi (concept-anchor :t))) ;=> true
  (reaction withi (concept-anchor :p)) ;=> nil
  (map :mark (quote-marks withi :n :text)) ;=> [nt]
  (quote-marks withi :n :summary) ;=> ()
  ;; on the graph, the agreement sits by its concept
  (map (comp :idea/id :idea) (:ideas (:ideas (graph-scene withi)))) ;=> [:i1]
  (:ideas (graph-scene (ideas-layer-toggle withi))) ;=> nil
  ;; a note from a text selection keeps the selection until saved or cancelled
  (def q {:anchor/kind :quote :quote "Bool"})
  (def drafting (-> withi (selection-made {:anchor q :x 1 :y 2}) (draft-start [q] true)))
  (get-in (selection-cleared drafting) [:ideas :selection :x]) ;=> 1
  (get-in (-> drafting (draft-link :i1) (draft-link :i2) (draft-link :i1)) [:ideas :draft :links]) ;=> #{:i2}
  (get-in (idea-added drafting {:idea/id :i3}) [:ideas :draft]) ;=> nil
  ;; what the notes say, derived: their own terms, the prerequisites they state
  (def c1 {:concept/id :c1 :concept/label "stream"})
  (def noted (ideas-fetched initial-state
                            [{:idea/id :n1 :idea/stance :note :idea/at 1 :idea/refs {"streams" c1}
                              :idea/text "[[streams]] needs [[Delayed evaluation]] and [[thunk]]."}
                             {:idea/id :n2 :idea/stance :restate :idea/at 2 :idea/refs {}
                              :idea/text "A [[delayed  evaluation]] waits."}
                             {:idea/id :n3 :idea/stance :agree :idea/at 3}]))
  (update-vals (my-terms noted) #(update % :notes (partial mapv :idea/id)))
  ;=> {"delayed evaluation" {:term "delayed evaluation" :label "Delayed evaluation" :notes [:n1 :n2]}
  ;    "thunk" {:term "thunk" :label "thunk" :notes [:n1]}}
  (mapv (juxt (comp note-ref-key :of) (comp note-ref-key :needs)) (my-claims noted))
  ;=> [[[:concept :c1] [:term "delayed evaluation"]] [[:concept :c1] [:term "thunk"]]]
  (-> (in-my-notes noted [:term "delayed evaluation"]) (update :notes (partial mapv :idea/id)) (update :needed-by (partial mapv note-ref-key)))
  ;=> {:needs [] :needed-by [[:concept :c1]] :notes [:n1 :n2]}
  (:view (open-term initial-state "Thunk ")) ;=> :term
  (:term (open-term initial-state "Thunk ")) ;=> "thunk"

  ;; the notebook: as the sources go (chapters in reading order), notes on concepts after; or newest first
  (defn- placed [id at order chapter page q text]
    {:idea/id id :idea/stance :note :idea/at at :idea/text text
     :idea/anchors (if q [{:anchor/kind :quote :quote q}] [])
     :idea/place {:source/id :s :source/title "SICP" :page page :order order
                  :path [{:node/id :root :node/title "SICP" :node/kind :source}
                         {:node/id chapter :node/title (name chapter) :node/kind :section}
                         {:node/id :p :node/title "A paragraph…" :node/kind :paragraph}]}})
  (def book (ideas-fetched initial-state
                           [(placed :a 3 [0 3 1] :ch3 200 "Streams are delayed lists." "So [[stream]]s wait.")
                            (placed :b 1 [0 1 0] :ch1 10 nil "Procedures first.")
                            {:idea/id :c :idea/stance :agree :idea/at 2 :idea/text "Yes."
                             :idea/targets [{:title "closure"}]}
                            (placed :d 4 [0 3 0] :ch3 190 nil "Earlier in chapter 3.")
                            {:idea/id :h :idea/stance :highlight :idea/at 5 :idea/anchors []}]))
  (mapv (fn [{:keys [title chapters]}] [title (mapv (fn [c] [(:title c) (mapv :idea/id (:entries c))]) chapters)])
        (notebook book))
  ;=> [["SICP" [["ch1" [:b]] ["ch3" [:d :a]]]] ["On concepts and relations" [[nil [:c]]]]]
  (mapv :idea/id (:entries (first (:chapters (first (notebook (notebook-order book :newest)))))))
  ;=> [:d :a :c :b]
  (count (mapcat :entries (mapcat :chapters (notebook (notebook-highlights book true)))))  ;=> 5
  (entry-where (placed :a 3 [0 3 1] :ch3 200 nil "x")) ;=> {:node/id :p :title "ch3" :page 200}
  (notebook-markdown (ideas-fetched initial-state [(placed :a 3 [0 3 1] :ch3 200 "Streams are\ndelayed lists." "So [[stream]]s wait.")]))
  ;=> "# Notebook\n\n## SICP\n\n### ch3\n\n> Streams are delayed lists.\n>\n> — p. 200, ch3\n\nSo [[stream]]s wait.\n"
  ;; rewriting a note: a draft holding it; stored, it replaces the old
  (get-in (draft-edit book {:idea/id :b :idea/stance :note :idea/text "x" :idea/links [:a]}) [:ideas :draft])
  ;=> {:editing :b :anchors [] :stance :note :text "x" :links #{:a} :from-selection? false}
  (:idea/text (second (get-in (idea-updated book {:idea/id :b :idea/text "y"}) [:ideas :all]))) ;=> "y"
  ;; one stance a note; a [[link]] being typed is looked up and completed
  (get-in (draft-stance drafting :restate) [:ideas :draft :stance]) ;=> :restate
  (get-in (draft-stance drafting :bookmark) [:ideas :draft :stance]) ;=> :note
  (wiki-term "A [[clo" 7) ;=> {:from 4 :term "clo"}
  (wiki-term "A [[closure]] b" 15) ;=> nil
  (wiki-term "A [[x\ny" 8) ;=> nil
  (def typing (-> drafting (draft-typed "See [[clo]] now" 9) (goto-fetched "cl" {:concepts [{:concept/label "closure"}]})))
  (wiki-query typing) ;=> "clo"
  (wiki-suggestions typing) ;=> [{:concept/label "closure"}]
  (let [[d caret] (wiki-picked typing "closure")] [(get-in d [:ideas :draft :text]) caret])
  ;=> ["See [[closure]] now" 15]
  (get-in (first (wiki-picked (draft-typed drafting "[[a" 3) "lambda")) [:ideas :draft :text]) ;=> "[[lambda]]"
  (map :idea/id (get-in (idea-removed withi :i1) [:ideas :all])) ;=> [:i2]
  ;; asking about a selection keeps it; the answer shows there and joins the asks
  (def asking (-> withi (selection-made {:anchor q :x 1 :y 2}) (selection-ask " Explain it ")))
  (get-in asking [:ideas :selection :ask]) ;=> {:question "Explain it" :status :asking}
  (get-in (selection-cleared asking) [:ideas :selection :x]) ;=> 1
  (get-in (selection-ask (selection-made withi {:anchor q}) "  ") [:ideas :selection :ask]) ;=> nil
  (def answered (selection-answered asking q "Explain it" {:ask/id :a1 :ask/answer "It is[1]."}))
  (get-in answered [:ideas :selection :ask :status]) ;=> :answered
  (map :ask/id (:ask-history answered)) ;=> [:a1]
  (get-in (selection-answered asking q "Other" {:ask/id :a2}) [:ideas :selection :ask :status]) ;=> :asking
  (get-in (set-error asking "boom") [:ideas :selection]) ;=> {:anchor q :x 1 :y 2}
  (answer-as-note "Why?" "Because[1] of it [2].") ;=> "Why?\n\nBecause of it."

  ;; routes
  (:view (apply-route initial-state {:view :study})) ;=> :study
  (:view (apply-route initial-state {:view :concept :id id})) ;=> :concept
  (:view (apply-route initial-state {:view :concepts})) ;=> :concepts
  (:view (apply-route initial-state {:view :ask :id id})) ;=> :ask
  (:view (apply-route initial-state {:view :keyword :keyword "confluence"})) ;=> :keyword
  (:view (apply-route (open-node initial-state id) {:view :home})) ;=> :home
  ;; back to a page of the source, or to the node's own page
  (get-in (apply-route initial-state {:view :node :id id :page 88}) [:doc :page]) ;=> 88
  (get-in (apply-route (doc-goto (open-node initial-state id) :s 9) {:view :node :id id}) [:doc :page])) ;=> nil

;;=============================================================================
;; Settings: adding sources, the models, the prompts
;;=============================================================================

(defn open-settings [db] (assoc db :view :settings :asking nil))

(defn ingest-active? [{:ingest/keys [status]}] (contains? #{:queued :running} status))

(defn ingesting?
  "Is a source being ingested?"
  [db]
  (boolean (some ingest-active? (get-in db [:settings :ingests]))))

(defn ingests-fetched [db jobs] (assoc-in db [:settings :ingests] (vec jobs)))

(defn prompts-fetched [db prompts] (assoc-in db [:settings :prompts] (vec prompts)))

(declare models-fetched)

(defn settings-fetched
  "What the settings page shows: {:settings :ingests :prompts} from the server."
  [db {:keys [settings ingests prompts]}]
  (-> db
      (models-fetched settings)
      (ingests-fetched ingests)
      (prompts-fetched prompts)))

(defn ingests-done
  "Every job finished (`jobs`): the knowledge changed, fetch it again."
  [db jobs]
  (-> db (ingests-fetched jobs) knowledge-changed))

;; --- files dropped ----------------------------------------------------------

(defn drag-over [db on?] (assoc-in db [:settings :drag?] (boolean on?)))

(defn- update-upload [db id f]
  (update-in db [:settings :uploads] (fn [us] (mapv #(if (= id (:id %)) (f %) %) us))))

(defn upload-started
  "File `file` (its name) dropped as upload `id`: being sent and read."
  [db id file]
  (-> db
      (update-in [:settings :uploads] (fnil conj []) {:id id :file file :status :uploading})
      (assoc-in [:settings :drag?] false)))

(defn upload-read
  "What upload `id` holds ({:candidates [..]}, server.ingests/upload!): each
  candidate readable is ingested unless unchecked, with all its parts."
  [db id {:keys [candidates]}]
  (update-upload db id #(assoc % :status :ready
                               :candidates (mapv (fn [c] (assoc c :include? (not (:error c)) :skip #{}))
                                                 candidates))))

(defn upload-failed [db id error] (update-upload db id #(assoc % :status :failed :error (str error))))

(defn upload-dismissed [db id]
  (update-in db [:settings :uploads] (fn [us] (filterv #(not= id (:id %)) us))))

(defn- update-candidate [db id path f]
  (update-upload db id (fn [u] (update u :candidates (fn [cs] (mapv #(if (= path (:path %)) (f %) %) cs))))))

(defn candidate-toggled [db id path]
  (update-candidate db id path #(update % :include? not)))

(defn part-toggled
  "Leave part `skip` of candidate `path` out of upload `id`, or in again."
  [db id path skip]
  (update-candidate db id path (fn [c] (update c :skip #(if (contains? % skip) (disj % skip) (conj % skip))))))

(defn upload-request
  "What ingesting upload `id` asks for: {:ingest/paths .. :ingest/skip ..},
  nil when nothing is checked."
  [db id]
  (let [cs (->> (get-in db [:settings :uploads]) (some #(when (= id (:id %)) %)) :candidates (filter :include?))]
    (when (seq cs)
      {:ingest/paths (mapv :path cs)
       :ingest/skip  (vec (sort (distinct (mapcat :skip cs))))})))

(defn ingest-started
  "Upload `id` is being ingested as `job`: its card gives way to the job."
  [db id job]
  (-> (upload-dismissed db id)
      (update-in [:settings :ingests] #(into [job] (remove (fn [j] (= (:ingest/id j) (:ingest/id job)))) %))))

(defn ingest-forgotten [db id]
  (update-in db [:settings :ingests] (fn [js] (filterv #(not= id (:ingest/id %)) js))))

;; --- prompts ----------------------------------------------------------------

(defn prompt-groups
  "The prompts by group, in the order they come: [[group [prompt ..]] ..]."
  [db]
  (let [ps (get-in db [:settings :prompts])]
    (for [g (distinct (map :prompt/group ps))]
      [g (filterv #(= g (:prompt/group %)) ps)])))

(defn prompt-opened
  "Edit prompt `id` (again: close it, its draft kept)."
  [db id]
  (update-in db [:settings :open] #(when-not (= % id) id)))

(defn prompt-typed [db id text] (assoc-in db [:settings :drafts id] text))

(defn prompt-draft
  "The text of prompt `p` being written: the draft, else its text."
  [db {:prompt/keys [id text]}]
  (get-in db [:settings :drafts id] text))

(defn prompt-changed?
  "Does the draft of prompt `p` differ from what is kept?"
  [db p]
  (not= (prompt-draft db p) (:prompt/text p)))

(defn prompt-dropped
  "The draft of prompt `id` thrown away."
  [db id]
  (-> db
      (update-in [:settings :drafts] dissoc id)
      (update-in [:settings :open] #(when-not (= % id) %))))

(defn prompt-saved
  "Prompt `p` kept as written."
  [db p]
  (-> db
      (update-in [:settings :prompts] (fn [ps] (mapv #(if (= (:prompt/id p) (:prompt/id %)) p %) ps)))
      (update-in [:settings :drafts] dissoc (:prompt/id p))))

^:rct/test
(comment
  (def dropped (upload-started (open-settings initial-state) :u1 "docs.zip"))
  (map :status (get-in dropped [:settings :uploads])) ;=> [:uploading]
  (def uploaded (upload-read dropped :u1 {:candidates [{:path "a" :type "markdown" :parts [{:title "Ref" :skip "Ref"}]}
                                                   {:path "b.pdf" :type "pdf"}
                                                   {:path "c.pdf" :error "Broken"}]}))
  (map :include? (get-in uploaded [:settings :uploads 0 :candidates])) ;=> [true true false]
  (upload-request (-> uploaded (part-toggled :u1 "a" "Ref") (candidate-toggled :u1 "b.pdf")) :u1)
  ;=> {:ingest/paths ["a"] :ingest/skip ["Ref"]}
  (upload-request (-> uploaded (candidate-toggled :u1 "a") (candidate-toggled :u1 "b.pdf")) :u1) ;=> nil
  (def started (ingest-started uploaded :u1 {:ingest/id :j1 :ingest/status :queued}))
  [(get-in started [:settings :uploads]) (ingesting? started)] ;=> [[] true]
  (ingesting? (ingests-fetched started [{:ingest/id :j1 :ingest/status :done}])) ;=> false
  (get-in (ingest-forgotten started :j1) [:settings :ingests]) ;=> []

  (def ps (prompts-fetched initial-state [{:prompt/id :a :prompt/group "Reading" :prompt/text "A"}
                                          {:prompt/id :b :prompt/group "Asking" :prompt/text "B"}
                                          {:prompt/id :c :prompt/group "Reading" :prompt/text "C"}]))
  (map (fn [[g xs]] [g (mapv :prompt/id xs)]) (prompt-groups ps)) ;=> [["Reading" [:a :c]] ["Asking" [:b]]]
  (def editing (-> ps (prompt-opened :a) (prompt-typed :a "A2")))
  [(prompt-draft editing {:prompt/id :a :prompt/text "A"}) (prompt-changed? editing {:prompt/id :a :prompt/text "A"})] ;=> ["A2" true]
  (get-in (prompt-saved editing {:prompt/id :a :prompt/group "Reading" :prompt/text "A2"}) [:settings :drafts]) ;=> {}
  (get-in (prompt-dropped editing :a) [:settings :open]) ;=> nil
  (:view (apply-route initial-state {:view :settings})) ;=> :settings
  )

;; --- models -----------------------------------------------------------------

(def provider-presets
  "Providers to add in one click: [label provider]."
  [["oMLX (this Mac)"          {:name "omlx" :type :openai :base-url "http://localhost:8000/v1"}]
   ["Ollama"                   {:name "ollama" :type :ollama :base-url "http://localhost:11434"}]
   ["LM Studio"                {:name "lmstudio" :type :openai :base-url "http://localhost:1234/v1"}]
   ["Anthropic"                {:name "anthropic" :type :anthropic}]
   ["Another OpenAI-compatible" {:name "custom" :type :openai :base-url "https://"}]])

(defn- tier-of
  "Tier model `ref` as {:provider :model}: \"p/id\" for provider p, else the default's."
  [ref names default]
  (let [[_ p id] (re-matches #"([^/]+)/(.+)" (str ref))]
    (if (contains? (set names) p) {:provider p :model id} {:provider default :model (str ref)})))

(defn models-draft
  "The model settings `view` as a draft to edit."
  [{:keys [providers default models]}]
  (let [names (map :name providers)]
    {:providers (mapv #(assoc % :api-key "" :forget-key? false) providers)
     :default   default
     :tiers     {:strong (tier-of (:strong models) names default)
                 :common (tier-of (:common models) names default)}}))

(defn models-fetched
  "The model settings arrived: shown, and edited from them (unless being edited)."
  [db view]
  (let [db (assoc-in db [:settings :models] view)]
    (if (and view (or (nil? (get-in db [:settings :draft]))
                      (= (get-in db [:settings :draft]) (get-in db [:settings :base]))))
      (let [d (models-draft view)]
        (-> db (assoc-in [:settings :draft] d) (assoc-in [:settings :base] d)))
      db)))

(defn models-saved
  "The model settings were saved as `view`: the draft starts from them."
  [db view]
  (let [d (models-draft view)]
    (-> db
        (assoc-in [:settings :models] view)
        (assoc-in [:settings :draft] d)
        (assoc-in [:settings :base] d)
        (assoc-in [:settings :tried] {}))))

(defn models-dirty? [db] (not= (get-in db [:settings :draft]) (get-in db [:settings :base])))

(defn models-discarded [db] (assoc-in db [:settings :draft] (get-in db [:settings :base])))

(defn- parse-count [v]
  (let [n (cond (number? v) v
                (and (string? v) (re-matches #"\s*\d+\s*" v)) #?(:clj (parse-long (clojure.string/trim v)) :cljs (js/parseInt v 10)))]
    (when (and n (pos? n)) n)))

(defn provider-set
  "Field `k` of the draft's provider `i` is now `v` (counts read as numbers)."
  [db i k v]
  (let [old (get-in db [:settings :draft :providers i :name])
        v   (if (#{:concurrency :num-ctx :timeout-s} k) (parse-count v) v)
        db  (assoc-in db [:settings :draft :providers i k] v)]
    (if (and (= k :name) old)
      ;; renamed: the default and the tiers follow it
      (-> db
          (update-in [:settings :draft :default] #(if (= % old) v %))
          (update-in [:settings :draft :tiers] (fn [ts] (into {} (for [[t x] ts] [t (if (= old (:provider x)) (assoc x :provider v) x)])))))
      db)))

(defn provider-added
  "Add provider `p` (a preset) to the draft, its name made unique."
  [db p]
  (let [names (set (map :name (get-in db [:settings :draft :providers])))
        n     (first (remove names (cons (:name p) (map #(str (:name p) "-" %) (iterate inc 2)))))]
    (update-in db [:settings :draft :providers] conj (assoc p :name n :api-key "" :forget-key? false :new? true))))

(defn provider-used?
  "Is the draft's provider `n` the default or a tier's?"
  [db n]
  (let [{:keys [default tiers]} (get-in db [:settings :draft])]
    (boolean (or (= n default) (some #(= n (:provider %)) (vals tiers))))))

(defn provider-removed [db i]
  (update-in db [:settings :draft :providers] #(vec (concat (subvec % 0 i) (subvec % (inc i))))))

(defn default-set [db n] (assoc-in db [:settings :draft :default] n))

(defn tier-set
  "The draft's tier `tier` (:strong :common): its :provider or :model is `v`."
  [db tier k v]
  (-> db
      (assoc-in [:settings :draft :tiers tier k] v)
      (update-in [:settings :tried] dissoc tier)))

(defn tier-ref
  "The model a draft tier names: \"provider/model\"."
  [{:keys [provider model]}]
  (str provider "/" model))

(defn models-request
  "The draft as the server saves it (server.settings/save!)."
  [db]
  (let [{:keys [providers default tiers]} (get-in db [:settings :draft])]
    {:providers (mapv #(select-keys % [:name :type :base-url :api-key-env :concurrency :num-ctx :timeout-s
                                       :api-key :forget-key?])
                      providers)
     :default   default
     :models    {:strong (tier-ref (:strong tiers)) :common (tier-ref (:common tiers))}}))

(defn listing [db n] (assoc-in db [:settings :listed n] {:status :listing}))

(defn listed
  "What provider `n` serves: {:models [..]} or {:problem ..}."
  [db n {:keys [models problem]}]
  (assoc-in db [:settings :listed n] (if problem {:status :failed :problem problem} {:status :ok :models models})))

(defn trying [db tier] (assoc-in db [:settings :tried tier] {:status :trying}))

(defn tried
  "How tier `tier`'s model answered: {:ok? :ms :reply :problem}."
  [db tier {:keys [ok?] :as r}]
  (assoc-in db [:settings :tried tier] (assoc (select-keys r [:ms :reply :problem]) :status (if ok? :ok :failed))))

^:rct/test
(comment
  (def view {:providers [{:name "anthropic" :type :anthropic :key {:from :keychain :hint "…bwAA"}}
                         {:name "omlx" :type :openai :base-url "http://localhost:8000/v1"}]
             :default "anthropic"
             :models {:strong "claude-sonnet-5-5" :common "omlx/mlx-community/Qwen3"}})
  (def loaded (models-fetched initial-state view))
  (get-in loaded [:settings :draft :tiers])
  ;=> {:strong {:provider "anthropic" :model "claude-sonnet-5-5"} :common {:provider "omlx" :model "mlx-community/Qwen3"}}
  (models-dirty? loaded) ;=> false
  (def edited (-> loaded (provider-set 1 :name "mlx") (provider-set 1 :concurrency "4") (provider-set 1 :api-key "sk-1")))
  [(models-dirty? edited) (get-in edited [:settings :draft :tiers :common :provider])] ;=> [true "mlx"]
  (= (models-request edited)
     {:providers [{:name "anthropic" :type :anthropic :api-key "" :forget-key? false}
                  {:name "mlx" :type :openai :base-url "http://localhost:8000/v1" :concurrency 4 :api-key "sk-1" :forget-key? false}]
      :default   "anthropic"
      :models    {:strong "anthropic/claude-sonnet-5-5" :common "mlx/mlx-community/Qwen3"}})
  ;=> true
  (map :name (get-in (provider-added loaded {:name "omlx" :type :openai}) [:settings :draft :providers])) ;=> ["anthropic" "omlx" "omlx-2"]
  [(provider-used? loaded "omlx") (provider-used? loaded "x")] ;=> [true false]
  ;; edited and fetched again: the edits stay
  (= (get-in (models-fetched edited view) [:settings :draft]) (get-in edited [:settings :draft])) ;=> true
  (models-dirty? (models-saved edited view)) ;=> false
  (get-in (tried loaded :common {:ok? false :problem "HTTP 401" :ms 20}) [:settings :tried :common :status]) ;=> :failed
  )
