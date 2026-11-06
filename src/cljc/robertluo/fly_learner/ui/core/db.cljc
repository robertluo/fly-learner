(ns robertluo.fly-learner.ui.core.db
  "Application state: pure db -> db updater functions.

  Every function takes db (and optional args) and returns the new db.
  Effects are declared in views as effect maps and executed by dispatch!
  (robertluo.fly-learner.ui.core)."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ui.core.graph :as graph]
            [robertluo.fly-learner.ui.core.layout :as layout]))

;;=============================================================================
;; State shape
;;=============================================================================

(def initial-state
  {:view            :home  ; :home | :node | :keyword | :ask | :concept | :concepts | :study
   :sources         []     ; [{:source/id :source/title :source/root {:node/id :node/summary}}]
   :node-id         nil    ; node shown in :node view
   :nodes           {}     ; node-id -> node (cache of fetched nodes)
   :keyword         nil    ; keyword shown in :keyword view
   :keyword-results {}     ; keyword -> [node-ref]
   :concept-id      nil    ; concept shown in :concept view
   :concepts        {}     ; concept-id -> concept (see robertluo.fly-learner.concept.view/concept)
   :concept-index   nil    ; [concept-ref] by label, once fetched (:concepts view)
   :concept-filter  ""     ; text filtering the index
   :graph           {:ix       nil  ; indexed concept graph (ui.core.graph/index), once fetched
                     :focus    nil  ; concept the shown graph grows from
                     :expanded #{}  ; concepts whose neighbours are shown
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
                     :draft     nil   ; {:anchors [..] :text "" :links #{}}: a note being written
                     :layer?    true} ; show my ideas on the concept graph
   :ask-id          nil    ; answer shown in :ask view
   :asking          nil    ; question waiting for its answer (:ask view, no :ask-id yet)
   :asks            {}     ; ask-id -> ask (see robertluo.fly-learner.ask/view)
   :ask-history     []     ; [{:ask/id :ask/question :ask/title :ask/at}], newest first
   :search-hits     {}     ; question -> [node-ref with :node/path] (full-text, no model)
   :pdf             {:page      nil   ; manual page (nil: follow the node)
                     :pages     {}    ; source-id -> page count
                     :highlight nil}  ; {:key [url page node-id] :aspect w/h :rects [{:x :y :w :h}]}
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
      (cond-> (= :asking (get-in db [:ideas :selection :ask :status]))
        (update-in [:ideas :selection] dissoc :ask))))

(defn clear-error [db] (assoc db :error nil))

;;=============================================================================
;; Navigation
;;=============================================================================

(defn go-home [db]
  (assoc db :view :home :node-id nil :keyword nil :asking nil))

(defn open-node
  "Show node `id`; the source pane follows the node's page again."
  [db id]
  (-> db
      (assoc :view :node :node-id id :keyword nil :asking nil)
      (assoc-in [:pdf :page] nil)
      (assoc :loading? (not (contains? (:nodes db) id)))))

(defn open-keyword [db kw]
  (-> db
      (assoc :view :keyword :keyword kw :asking nil)
      (assoc :loading? (not (contains? (:keyword-results db) kw)))))

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

(defn apply-route
  "Apply a parsed URL ({:view .. :id .. :keyword ..}, see history) to db."
  [db {:keys [view id keyword]}]
  (case view
    :node    (open-node db id)
    :keyword (open-keyword db keyword)
    :ask      (open-ask db id)
    :concept  (open-concept db id)
    :concepts (open-concepts db)
    :study    (open-study db)
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
  "Ask a question on `item` (nil: what to study next), on the study page."
  [db item]
  (-> (open-study db)
      (assoc :quiz {:status :asking :item item :question nil :response "" :result nil})))

(defn question-created [db question]
  (if (= :asking (get-in db [:quiz :status]))
    (update db :quiz assoc :status :answering :question question :item (:question/item question))
    db))

(defn quiz-typed [db s] (assoc-in db [:quiz :response] s))

(defn quiz-submitted [db] (assoc-in db [:quiz :status] :grading))

(defn quiz-graded
  "The grading arrived: show it, and take the new scores it carries."
  [db result]
  (cond-> (update db :quiz assoc :status :graded :result result)
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

(defn idea-removed [db id]
  (update-in db [:ideas :all] (fn [all] (vec (remove #(= id (:idea/id %)) all)))))

(defn- target-keys [idea] (set (map :key (:idea/targets idea))))

(defn ideas-on
  "Ideas about any target in `keys` (e.g. #{[:concept id]}), oldest first."
  [db keys]
  (filterv #(some keys (target-keys %)) (get-in db [:ideas :all])))

(defn ideas-in-node
  "Ideas on node `id`, on anything inside it, or on the PDF page `page-key`
  ([:page source-id page]) shown beside it."
  [db id page-key]
  (filterv #(or (some #{[:node id] page-key} (target-keys %))
                (some #{id} (:idea/within %)))
           (get-in db [:ideas :all])))

(defn ideas-about-concept
  "Ideas on concept `id` and on its relations."
  [db id]
  (filterv #(some (fn [[kind a b]] (or (= [kind a] [:concept id])
                                       (and (= :relation kind) (or (= a id) (= b id)))))
                  (target-keys %))
           (get-in db [:ideas :all])))

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
   (assoc-in db [:ideas :draft] {:anchors (vec anchors) :text text :links #{} :from-selection? from-selection?})))

(defn draft-typed [db s] (assoc-in db [:ideas :draft :text] s))

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

(defn graph-relayout
  "Lay out what is visible now. Concepts already shown start where they are
  drawn; new ones grow out of the concept that revealed them."
  [db]
  (let [{:keys [ix focus expanded] :as g} (:graph db)]
    (if-not (and ix focus)
      db
      (let [vis   (graph/visible ix focus expanded)
            shown (select-keys (shown-positions g) vis)
            seeds (into {} (for [id vis :when (not (shown id))]
                             [id (graph/revealer ix vis expanded id)]))]
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
      (update :graph assoc :focus id :expanded #{id} :pos {} :from {})
      graph-relayout))

(defn show-in-graph
  "Keep the graph if `id` is visible in it, else restart it from `id`."
  [db id]
  (let [{:keys [ix focus expanded]} (:graph db)]
    (if (and ix focus (contains? (graph/visible ix focus expanded) id))
      db
      (graph-reset db id))))

(defn graph-toggle [db id]
  (-> db
      (update-in [:graph :expanded] #(if (contains? % id) (disj % id) (conj % id)))
      graph-relayout))

(defn graph-click
  "Clicking a concept in the graph opens it and expands it; clicking the
  open concept again collapses or re-expands it."
  [db id]
  (cond
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
        vis (if (and ix focus) (graph/visible ix focus expanded) #{})
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
                                  :hidden (graph/hidden-count ix vis id)}))))
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

(defn keyword-fetched [db kw nodes]
  (-> db
      (assoc-in [:keyword-results kw] (vec nodes))
      (assoc :loading? false)))

;;=============================================================================
;; Source pane (pdf.js)
;;=============================================================================

(defn node-page
  "Page a node points at in its source: its own page, else its first child's."
  [node]
  (or (:node/page node)
      (some :node/page (:node/children node))
      1))

(defn pdf-page
  "Page shown in the source pane."
  [db]
  (or (get-in db [:pdf :page])
      (some-> (current-node db) node-page)
      1))

(defn pdf-pages [db source-id] (get-in db [:pdf :pages source-id]))

(defn pdf-loaded [db source-id n]
  (assoc-in db [:pdf :pages source-id] n))

(defn pdf-goto
  "Show page `p` (clamped to the document) regardless of the selected node."
  [db source-id p]
  (let [n (or (pdf-pages db source-id) p)]
    (assoc-in db [:pdf :page] (-> p (max 1) (min n)))))

(defn pdf-follow [db] (assoc-in db [:pdf :page] nil))

(defn following? [db] (nil? (get-in db [:pdf :page])))

(defn highlight-texts
  "Source text of `node` to mark on its PDF page: a paragraph's text, a
  passage's paragraphs, a section's heading."
  [{:node/keys [kind text title children]}]
  (case kind
    :paragraph (when text [text])
    :group     (vec (keep :node/text children))
    :section   (when title [title])
    nil))

(defn highlight-key
  "Identifies the highlight wanted now: document, page and node."
  [db url]
  [url (pdf-page db) (:node-id db)])

(defn highlight
  "The highlight for `key` once computed (rects as fractions of the page)."
  [db key]
  (let [h (get-in db [:pdf :highlight])]
    (when (= key (:key h)) h)))

(defn highlight-found [db key result]
  (assoc-in db [:pdf :highlight] (assoc result :key key)))

^:rct/test
(comment
  (def id #uuid "00000000-0000-0000-0000-000000000001")
  (def n {:node/id id :node/page 7 :node/children [{:node/page 8}]
          :node/source {:source/id :s}})

  ;; opening an uncached node shows loading; cached does not
  (:loading? (open-node initial-state id)) ;=> true
  (:loading? (-> initial-state (node-fetched n) (open-node id))) ;=> false

  ;; the pane follows the node's page, a section falls back to its first child's
  (pdf-page (-> initial-state (open-node id) (node-fetched n))) ;=> 7
  (node-page {:node/children [{:node/page 3}]}) ;=> 3
  (node-page {}) ;=> 1

  ;; manual paging is clamped and survives until the next node is opened
  (def paged (-> initial-state (open-node id) (node-fetched n) (pdf-loaded :s 10) (pdf-goto :s 42)))
  (pdf-page paged) ;=> 10
  (following? paged) ;=> false
  (pdf-page (open-node paged id)) ;=> 7

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

  ;; a quiz: ask, answer, grade; new scores come with the grading
  (def asked (quiz-start initial-state [:concept :t]))
  [(:view asked) (get-in asked [:quiz :status])] ;=> [:study :asking]
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
  (:view (apply-route (open-node initial-state id) {:view :home}))) ;=> :home
