(ns robertluo.fly-learner.ui.core.db
  "Application state: pure db -> db updater functions.

  Every function takes db (and optional args) and returns the new db.
  Effects are declared in views as effect maps and executed by dispatch!
  (robertluo.fly-learner.ui.core)."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.graph :as graph]
            [robertluo.fly-learner.ui.core.layout :as layout]))

;;=============================================================================
;; State shape
;;=============================================================================

(def initial-state
  {:view            :home  ; :home | :node | :keyword | :ask | :concept | :concepts
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
  (assoc db :loading? false :asking nil
         :error (if (map? error) error {:code :unknown :reason (str error)})))

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

(declare show-in-graph)

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
                             {:pad 24})]
    {:nodes (->> vis
                 (keep (fn [id]
                         (when-let [[x y] (pos id)]
                           (let [{:concept/keys [label gloss mentions]} (get-in ix [:concepts id])]
                             {:id id :label label :gloss gloss :mentions mentions
                              :x x :y y :r (radius mentions)
                              :focus? (= id focus) :selected? (= id sel)
                              :expanded? (contains? expanded id)
                              :hidden (graph/hidden-count ix vis id)}))))
                 ;; the selected concept is drawn last, on top
                 (sort-by (juxt :selected? :label))
                 vec)
     :edges (vec (for [{:keys [from to] :as e} (graph/edges ix vis)
                       :let [[x1 y1] (pos from), [x2 y2] (pos to)]
                       :when (and x1 x2)]
                   (assoc e :x1 x1 :y1 y1 :x2 x2 :y2 y2)))
     :box   box
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

  ;; routes
  (:view (apply-route initial-state {:view :concept :id id})) ;=> :concept
  (:view (apply-route initial-state {:view :concepts})) ;=> :concepts
  (:view (apply-route initial-state {:view :ask :id id})) ;=> :ask
  (:view (apply-route initial-state {:view :keyword :keyword "confluence"})) ;=> :keyword
  (:view (apply-route (open-node initial-state id) {:view :home}))) ;=> :home
