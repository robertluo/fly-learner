(ns learner.ui.core.db
  "Application state: pure db -> db updater functions.

  Every function takes db (and optional args) and returns the new db.
  Effects are declared in views as effect maps and executed by dispatch!
  (learner.ui.core).")

;;=============================================================================
;; State shape
;;=============================================================================

(def initial-state
  {:view            :home  ; :home | :node | :keyword | :ask
   :sources         []     ; [{:source/id :source/title :source/root {:node/id :node/summary}}]
   :node-id         nil    ; node shown in :node view
   :nodes           {}     ; node-id -> node (cache of fetched nodes)
   :keyword         nil    ; keyword shown in :keyword view
   :keyword-results {}     ; keyword -> [node-ref]
   :ask-id          nil    ; answer shown in :ask view
   :asking          nil    ; question waiting for its answer (:ask view, no :ask-id yet)
   :asks            {}     ; ask-id -> ask (see learner.ask/view)
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

(defn current-ask [db]
  (get-in db [:asks (:ask-id db)]))

(defn apply-route
  "Apply a parsed URL ({:view .. :id .. :keyword ..}, see history) to db."
  [db {:keys [view id keyword]}]
  (case view
    :node    (open-node db id)
    :keyword (open-keyword db keyword)
    :ask     (open-ask db id)
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

  ;; routes
  (:view (apply-route initial-state {:view :ask :id id})) ;=> :ask
  (:view (apply-route initial-state {:view :keyword :keyword "confluence"})) ;=> :keyword
  (:view (apply-route (open-node initial-state id) {:view :home}))) ;=> :home
