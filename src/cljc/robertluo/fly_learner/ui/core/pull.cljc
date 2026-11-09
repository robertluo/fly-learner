(ns robertluo.fly-learner.ui.core.pull
  "What the UI fetches from the backend, when, and where the answers go.

  Each operation resolves to {:request .. :then ..}: :request is a request
  (robertluo.fly-learner.ui.core.api, which alone knows the wire format);
  :then is (fn [answer] effect-map). Named operations (`resolve-pull`) read
  what to fetch from the state; the rest take arguments (`idea-create`,
  `idea-update`, `idea-delete`, `goal`, `concept-delete`, `topic-delete`). Pure data functions, testable on the JVM."
  (:require [robertluo.fly-learner.ui.core.api :as api]
            [robertluo.fly-learner.ui.core.db :as db]))

(defn resolve-pull
  "Resolve a named pull operation against db. nil when nothing needs fetching."
  [op db]
  (case op
    :init
    {:request (api/batch {:sources (api/fetch :sources) :asks (api/fetch :asks) :universe (api/fetch :universe)})
     :then    (fn [{:keys [sources asks universe]}]
                {:db #(-> %
                          (db/sources-fetched sources)
                          (db/history-fetched asks)
                          (db/universe-fetched universe))})}

    ;; every cluster with its score (the map; the header's clusters being learned)
    :universe
    (when (or (nil? (:universe db)) (get-in db [:universe :stale?]))
      {:request (api/fetch :universe)
       :then    (fn [u] {:db #(db/universe-fetched % u)})})

    ;; what to study next in the cluster shown
    :cluster
    (let [id (:cluster-id db)]
      (when (and id (not (contains? (:clusters db) id)))
        {:request (api/fetch :clusters {:cluster/id id})
         :then    (fn [c] {:db #(db/cluster-fetched % id c)})}))

    :ask
    (let [id (:ask-id db)]
      (when (and id (not (contains? (:asks db) id)))
        {:request (api/fetch :asks {:ask/id id})
         :then    (fn [ask] {:db #(db/ask-fetched % ask)})}))

    ;; answer the question being asked (a create: runs the model, stores the answer)
    :ask-create
    (when-let [q (:asking db)]
      {:request (api/create :asks {:ask/question q})
       :then    (fn [ask] {:db #(db/ask-created % q ask) :history :push})})

    ;; instant full-text matches for the question being asked
    :search
    (when-let [q (:asking db)]
      (when-not (contains? (:search-hits db) q)
        {:request (api/fetch :search {:q q})
         :then    (fn [hits] {:db #(db/search-fetched % q hits)})}))

    ;; what the top box's text names, as it is typed
    :goto
    (when-let [q (db/goto-query db)]
      (when-not (contains? (:goto db) q)
        {:request (api/fetch :goto {:q q})
         :then    (fn [found] {:db #(db/goto-fetched % q found)})}))

    ;; concepts a [[link]] being written may name (the top box's lookup)
    :wiki
    (when-let [q (db/wiki-query db)]
      (when-not (contains? (:goto db) q)
        {:request (api/fetch :goto {:q q})
         :then    (fn [found] {:db #(db/goto-fetched % q found)})}))

    :node
    (let [id (:node-id db)]
      (when (and id (not (contains? (:nodes db) id)))
        {:request (api/fetch :nodes {:node/id id})
         :then    (fn [node] {:db #(db/node-fetched % node) :pull :keypoints})}))

    ;; the key sentences of the source of the node shown, once
    :keypoints
    (let [sid (get-in (db/current-node db) [:node/source :source/id])]
      (when (and sid (not (contains? (:keypoints db) sid)))
        {:request (api/fetch :keypoints {:source/id sid})
         :then    (fn [k] {:db #(db/keypoints-fetched % sid k)})}))

    :exploration
    (let [id (:exploration-id db)]
      (when (and (uuid? id) (not (contains? (:explorations db) id)))
        {:request (api/fetch :explorations {:exploration/id id})
         :then    (fn [x] {:db #(db/exploration-fetched % x)})}))

    :explorations
    (when-not (:exploration-list db)
      {:request (api/fetch :explorations)
       :then    (fn [all] {:db #(db/exploration-list-fetched % all)})})

    ;; make an exploration (a create: runs the model, about half a minute)
    :exploration-create
    (when-let [{:keys [node previous feedback]} (:exploring db)]
      {:request (api/create :explorations (cond-> {}
                                            node     (assoc :exploration/node node)
                                            previous (assoc :exploration/previous previous)
                                            (seq feedback) (assoc :exploration/feedback feedback)))
       :then    (fn [x] {:db #(db/exploration-made % x) :history :push})})

    ;; remove the exploration shown
    :exploration-delete
    (let [id (:exploration-id db)]
      (when (uuid? id)
        {:request (api/delete :explorations {:exploration/id id})
         :then    (fn [_] {:db #(db/exploration-deleted % id) :history :push})}))

    ;; the left pane follows the source: the node beside the page scrolled to
    :page-sync
    (when-let [{:keys [page] :as q} (db/page-sync db)]
      ;; the node comes whole: shown at once, the source never unmounted for a "Loading…"
      {:request (api/fetch :pages q)
       :then    (fn [node]
                  (when (and node (not= (:node/id node) (:near q)))
                    {:db #(-> % (db/node-fetched node) (db/page-synced page (:node/id node)))
                     :pull :ideas :history :replace}))})

    :concept
    (let [id (:concept-id db)]
      (when (and id (not (contains? (:concepts db) id)))
        {:request (api/fetch :concepts {:concept/id id})
         :then    (fn [concept] {:db #(db/concept-fetched % concept)})}))

    :concept-graph
    (when-not (get-in db [:graph :ix])
      {:request (api/fetch :concept-graph)
       :then    (fn [g] {:db #(db/graph-fetched % g) :animate true})})

    :learning
    (when-not (:learning db)
      {:request (api/fetch :learning)
       :then    (fn [l] {:db #(db/learning-fetched % l)})})

    ;; write a question on the quiz's item (a create: runs the model)
    :question-create
    (when (= :asking (get-in db [:quiz :status]))
      {:request (api/create :questions (let [{:keys [item exercise]} (:quiz db)]
                                         (cond-> {:question/item item} exercise (assoc :question/exercise exercise))))
       :then    (fn [q] {:db #(db/question-created % q)})})

    ;; grade the answer (a create: runs the model, records evidence)
    :answer-create
    (let [{:keys [status question response]} (:quiz db)]
      (when (= :grading status)
        {:request (api/create :answers {:question/id (:question/id question) :response response})
         :then    (fn [graded] {:db #(db/quiz-graded % graded)})}))

    ;; answer a question about the selected text (a create: runs the model)
    :selection-ask
    (let [{:keys [anchor ask]} (get-in db [:ideas :selection])]
      (when (= :asking (:status ask))
        {:request (api/create :asks {:ask/question (:question ask) :ask/anchor anchor})
         :then    (fn [answer] {:db #(db/selection-answered % anchor (:question ask) answer)})}))

    :ideas
    (when-not (get-in db [:ideas :all])
      {:request (api/fetch :ideas)
       :then    (fn [all] {:db #(db/ideas-fetched % all)})})

    ;; add the concept the form holds (a create: finds its passages, its topic; may run the model)
    :concept-create
    (when-let [c (and (get-in db [:concept-form :adding?]) (db/concept-form-value db))]
      {:request (api/create :concepts c)
       :then    (fn [concept] {:db #(db/concept-added % concept) :history :push
                               :pull [:universe :concept-graph :learning :ideas]})})

    :concept-index
    (when-not (:concept-index db)
      {:request (api/fetch :concepts)
       :then    (fn [all] {:db #(db/concept-index-fetched % all)})})

    ;; the passages naming the learner's term shown
    :named
    (let [t (:term db)]
      (when (and (= :term (:view db)) t (not (contains? (:named db) t)))
        {:request (api/fetch :named {:term t})
         :then    (fn [nodes] {:db #(db/named-fetched % t nodes)})}))

    :keyword
    (let [kw (:keyword db)]
      (when (and kw (not (contains? (:keyword-results db) kw)))
        {:request (api/fetch :keywords {:keyword kw})
         :then    (fn [nodes] {:db #(db/keyword-fetched % kw nodes)})}))

    nil))

(defn idea-create
  "Keep a new idea (`data`, see robertluo.fly-learner.ideas)."
  [data]
  {:request (api/create :ideas data)
   :then    (fn [idea] {:db #(db/idea-added % idea)})})

(defn idea-update
  "Rewrite idea `id` as `data` ({:idea/stance :idea/text :idea/links})."
  [id data]
  {:request (api/change :ideas {:idea/id id} data)
   :then    (fn [idea] {:db #(db/idea-updated % idea)})})

(defn idea-delete
  "Remove idea `id` (the state lets go of it at once: db/idea-removed)."
  [id]
  {:request (api/delete :ideas {:idea/id id})
   :then    (fn [_] {})})

(defn goal
  "Learn cluster `id` now (`on?`), or stop; then what to study next again."
  [id on?]
  {:request (if on? (api/create :goals {:cluster/id id}) (api/delete :goals {:cluster/id id}))
   :then    (fn [_] {:pull :learning})})

(defn concept-delete
  "Concept `id` is no concept: remove it (its names are ignored from then on)."
  [id]
  {:request (api/delete :concepts {:concept/id id})
   :then    (fn [_] {:db #(db/concept-removed % id) :history :push :pull [:concept-index :universe]})})

(defn topic-delete
  "Topic `id` is no knowledge: remove it and its concepts."
  [id]
  {:request (api/delete :clusters {:cluster/id id})
   :then    (fn [_] {:db #(db/go-home (db/knowledge-changed %)) :history :push :pull [:universe :learning :ideas]})})

(defn for-route
  "Pull operation needed after navigating to a parsed route."
  [{:keys [view]}]
  (case view
    :home     [:universe :learning :ideas]
    :cluster  [:universe :cluster :concept-graph :learning :ideas]
    :node     [:node :ideas]
    :keyword  :keyword
    :term     [:named :ideas]
    :notebook :ideas
    :ask      :ask
    :concept  [:universe :concept :concept-graph :learning :ideas]
    :concepts [:concept-index :ideas]
    :study    :learning
    :explore  [:exploration :explorations]
    nil))

^:rct/test
(comment
  (def id #uuid "00000000-0000-0000-0000-000000000001")

  (-> (resolve-pull :init db/initial-state) :request :requests keys set) ;=> #{:sources :asks :universe}
  ;; :then gets the answers by name
  (let [d ((:db ((:then (resolve-pull :init db/initial-state)) {:sources [{:source/id id}] :asks [] :universe nil}))
           db/initial-state)]
    (:sources d))
  ;=> [{:source/id #uuid "00000000-0000-0000-0000-000000000001"}]

  ;; asking: create the answer and search right away; nothing when not asking
  (:request (resolve-pull :ask-create (db/ask-submitted db/initial-state "Why?")))
  ;=> {:op :create :coll :asks :value {:ask/question "Why?"}}
  (:request (resolve-pull :search (db/ask-submitted db/initial-state "Why?")))
  ;=> {:op :fetch :coll :search :key {:q "Why?"}}
  (resolve-pull :ask-create db/initial-state) ;=> nil

  ;; uncached node -> fetch it; cached -> nothing to do
  (= (api/fetch :nodes {:node/id id})
     (:request (resolve-pull :node (db/open-node db/initial-state id))))
  ;=> true
  (resolve-pull :node (-> db/initial-state (db/node-fetched {:node/id id}) (db/open-node id))) ;=> nil

  ;; :then stores the node
  (let [db  (db/open-node db/initial-state id)
        eff ((:then (resolve-pull :node db)) {:node/id id :node/title "T"})]
    (:node/title (db/current-node ((:db eff) db))))
  ;=> "T"

  (= (api/fetch :keywords {:keyword "confluence"})
     (:request (resolve-pull :keyword (db/open-keyword db/initial-state "confluence"))))
  ;=> true

  (= (api/fetch :concepts {:concept/id id})
     (:request (resolve-pull :concept (db/open-concept db/initial-state id))))
  ;=> true
  (resolve-pull :concept-index (db/concept-index-fetched db/initial-state [])) ;=> nil

  (for-route {:view :node}) ;=> [:node :ideas]
  (for-route {:view :concepts}) ;=> [:concept-index :ideas]
  (for-route {:view :term}) ;=> [:named :ideas]
  (:request (resolve-pull :named (db/open-term db/initial-state "Thunk"))) ;=> (api/fetch :named {:term "thunk"})
  (for-route {:view :concept}) ;=> [:universe :concept :concept-graph :learning :ideas]
  (for-route {:view :cluster}) ;=> [:universe :cluster :concept-graph :learning :ideas]
  (for-route {:view :home}) ;=> [:universe :learning :ideas]
  ;; the map is fetched once, and again after scores moved
  (resolve-pull :universe (db/universe-fetched db/initial-state {:clusters [] :links []})) ;=> nil
  (some? (resolve-pull :universe (assoc-in (db/universe-fetched db/initial-state {:clusters [] :links []}) [:universe :stale?] true))) ;=> true
  (for-route {:view :study}) ;=> :learning
  (:request (resolve-pull :question-create (db/quiz-start db/initial-state [:concept id])))
  ;=> {:op :create :coll :questions :value {:question/item [:concept #uuid "00000000-0000-0000-0000-000000000001"]}}
  ;; a book exercise of it
  (get-in (:request (resolve-pull :question-create (db/quiz-start db/initial-state [:concept id] id)))
          [:value :question/exercise])
  ;=> #uuid "00000000-0000-0000-0000-000000000001"
  (resolve-pull :answer-create db/initial-state) ;=> nil
  (:request (resolve-pull :concept-graph db/initial-state)) ;=> {:op :fetch :coll :concept-graph}
  ;; learning a topic, or not
  (:request (goal id true)) ;=> {:op :create :coll :goals :value {:cluster/id #uuid "00000000-0000-0000-0000-000000000001"}}
  (:request (goal id false)) ;=> {:op :delete :coll :goals :key {:cluster/id #uuid "00000000-0000-0000-0000-000000000001"}}
  (for-route {:view :home})) ;=> nil
