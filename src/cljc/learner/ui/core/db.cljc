(ns learner.ui.core.db
  "Application state: pure db -> db updater functions.

  Every function takes db (and optional args) and returns the new db.
  Effects are declared in views as effect maps and executed by dispatch!
  (learner.ui.core).")

;;=============================================================================
;; State shape
;;=============================================================================

(def initial-state
  {:view            :home  ; :home | :node | :keyword
   :sources         []     ; [{:source/id :source/title :source/root {:node/id :node/summary}}]
   :node-id         nil    ; node shown in :node view
   :nodes           {}     ; node-id -> node (cache of fetched nodes)
   :keyword         nil    ; keyword shown in :keyword view
   :keyword-results {}     ; keyword -> [node-ref]
   :pdf             {:page nil :pages {}} ; :page = manual page (nil: follow node); :pages source-id -> count
   :loading?        false
   :error           nil})  ; {:code :reason} or nil

(defn current-node [db]
  (get-in db [:nodes (:node-id db)]))

;;=============================================================================
;; Loading / errors
;;=============================================================================

(defn set-loading [db] (assoc db :loading? true :error nil))

(defn set-error [db error]
  (assoc db :loading? false
         :error (if (map? error) error {:code :unknown :reason (str error)})))

(defn clear-error [db] (assoc db :error nil))

;;=============================================================================
;; Navigation
;;=============================================================================

(defn go-home [db]
  (assoc db :view :home :node-id nil :keyword nil))

(defn open-node
  "Show node `id`; the source pane follows the node's page again."
  [db id]
  (-> db
      (assoc :view :node :node-id id :keyword nil)
      (assoc-in [:pdf :page] nil)
      (assoc :loading? (not (contains? (:nodes db) id)))))

(defn open-keyword [db kw]
  (-> db
      (assoc :view :keyword :keyword kw)
      (assoc :loading? (not (contains? (:keyword-results db) kw)))))

(defn apply-route
  "Apply a parsed URL ({:view .. :id .. :keyword ..}, see history) to db."
  [db {:keys [view id keyword]}]
  (case view
    :node    (open-node db id)
    :keyword (open-keyword db keyword)
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

  ;; missing node -> error
  (:code (:error (node-fetched initial-state nil))) ;=> :not-found

  ;; routes
  (:view (apply-route initial-state {:view :keyword :keyword "confluence"})) ;=> :keyword
  (:view (apply-route (open-node initial-state id) {:view :home}))) ;=> :home
