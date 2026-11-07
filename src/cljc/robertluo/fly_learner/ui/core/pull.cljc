(ns robertluo.fly-learner.ui.core.pull
  "Pull spec definitions: pattern + :then as data.

  Each named operation resolves to {:pattern ... :then ...}. :pattern is sent
  to /api (see robertluo.fly-learner.server.api); :then is (fn [response] effect-map).
  Pure data functions, testable on the JVM."
  (:require [robertluo.fly-learner.ui.core.db :as db]))

(defn resolve-pull
  "Resolve a named pull operation against db. nil when nothing needs fetching."
  [op db]
  (case op
    :init
    {:pattern '{:sources ?sources :asks ?asks :universe ?u}
     :then    (fn [r] {:db #(-> %
                                (db/sources-fetched (get r 'sources))
                                (db/history-fetched (get r 'asks))
                                (db/universe-fetched (get r 'u)))})}

    ;; every cluster with its score (the map; the header's clusters being learned)
    :universe
    (when (or (nil? (:universe db)) (get-in db [:universe :stale?]))
      {:pattern '{:universe ?u}
       :then    (fn [r] {:db #(db/universe-fetched % (get r 'u))})})

    ;; what to study next in the cluster shown
    :cluster
    (let [id (:cluster-id db)]
      (when (and id (not (contains? (:clusters db) id)))
        {:pattern {:clusters {{:cluster/id id} '?c}}
         :then    (fn [r] {:db #(db/cluster-fetched % id (get r 'c))})}))

    :ask
    (let [id (:ask-id db)]
      (when (and id (not (contains? (:asks db) id)))
        {:pattern {:asks {{:ask/id id} '?ask}}
         :then    (fn [r] {:db #(db/ask-fetched % (get r 'ask))})}))

    ;; answer the question being asked (a create: runs the model, stores the answer)
    :ask-create
    (when-let [q (:asking db)]
      {:pattern {:asks {nil {:ask/question q}}}
       :then    (fn [r] {:db #(db/ask-created % q (get r 'asks)) :history :push})})

    ;; instant full-text matches for the question being asked
    :search
    (when-let [q (:asking db)]
      (when-not (contains? (:search-hits db) q)
        {:pattern {:search {{:q q} '?hits}}
         :then    (fn [r] {:db #(db/search-fetched % q (get r 'hits))})}))

    :node
    (let [id (:node-id db)]
      (when (and id (not (contains? (:nodes db) id)))
        {:pattern {:nodes {{:node/id id} '?node}}
         :then    (fn [r] {:db #(db/node-fetched % (get r 'node)) :pull :keypoints})}))

    ;; the key sentences of the source of the node shown, once
    :keypoints
    (let [sid (get-in (db/current-node db) [:node/source :source/id])]
      (when (and sid (not (contains? (:keypoints db) sid)))
        {:pattern {:keypoints {{:source/id sid} '?k}}
         :then    (fn [r] {:db #(db/keypoints-fetched % sid (get r 'k))})}))

    :exploration
    (let [id (:exploration-id db)]
      (when (and (uuid? id) (not (contains? (:explorations db) id)))
        {:pattern {:explorations {{:exploration/id id} '?x}}
         :then    (fn [r] {:db #(db/exploration-fetched % (get r 'x))})}))

    :explorations
    (when-not (:exploration-list db)
      {:pattern '{:explorations ?all}
       :then    (fn [r] {:db #(db/exploration-list-fetched % (get r 'all))})})

    ;; make an exploration (a create: runs the model, about half a minute)
    :exploration-create
    (when-let [{:keys [node previous feedback]} (:exploring db)]
      {:pattern {:explorations {nil (cond-> {}
                                      node     (assoc :exploration/node node)
                                      previous (assoc :exploration/previous previous)
                                      (seq feedback) (assoc :exploration/feedback feedback))}}
       :then    (fn [r] {:db #(db/exploration-made % (get r 'explorations)) :history :push})})

    ;; remove the exploration shown
    :exploration-delete
    (let [id (:exploration-id db)]
      (when (uuid? id)
        {:pattern {:explorations {{:exploration/id id} nil}}
         :then    (fn [_] {:db #(db/exploration-deleted % id) :history :push})}))

    ;; the left pane follows the source: the node beside the page scrolled to
    :page-sync
    (when-let [{:keys [page] :as q} (db/page-sync db)]
      ;; the node comes whole: shown at once, the source never unmounted for a "Loading…"
      {:pattern {:pages {q '?n}}
       :then    (fn [r]
                  (let [node (get r 'n)]
                    (when (and node (not= (:node/id node) (:near q)))
                      {:db #(-> % (db/node-fetched node) (db/page-synced page (:node/id node)))
                       :pull :ideas :history :replace})))})

    :concept
    (let [id (:concept-id db)]
      (when (and id (not (contains? (:concepts db) id)))
        {:pattern {:concepts {{:concept/id id} '?concept}}
         :then    (fn [r] {:db #(db/concept-fetched % (get r 'concept))})}))

    :concept-graph
    (when-not (get-in db [:graph :ix])
      {:pattern '{:concept-graph ?g}
       :then    (fn [r] {:db #(db/graph-fetched % (get r 'g)) :animate true})})

    :learning
    (when-not (:learning db)
      {:pattern '{:learning ?l}
       :then    (fn [r] {:db #(db/learning-fetched % (get r 'l))})})

    ;; write a question on the quiz's item (a create: runs the model)
    :question-create
    (when (= :asking (get-in db [:quiz :status]))
      {:pattern {:questions {nil (let [{:keys [item exercise]} (:quiz db)]
                                   (cond-> {:question/item item} exercise (assoc :question/exercise exercise)))}}
       :then    (fn [r] {:db #(db/question-created % (get r 'questions))})})

    ;; grade the answer (a create: runs the model, records evidence)
    :answer-create
    (let [{:keys [status question response]} (:quiz db)]
      (when (= :grading status)
        {:pattern {:answers {nil {:question/id (:question/id question) :response response}}}
         :then    (fn [r] {:db #(db/quiz-graded % (get r 'answers))})}))

    ;; answer a question about the selected text (a create: runs the model)
    :selection-ask
    (let [{:keys [anchor ask]} (get-in db [:ideas :selection])]
      (when (= :asking (:status ask))
        {:pattern {:asks {nil {:ask/question (:question ask) :ask/anchor anchor}}}
         :then    (fn [r] {:db #(db/selection-answered % anchor (:question ask) (get r 'asks))})}))

    :ideas
    (when-not (get-in db [:ideas :all])
      {:pattern '{:ideas ?all}
       :then    (fn [r] {:db #(db/ideas-fetched % (get r 'all))})})

    :concept-index
    (when-not (:concept-index db)
      {:pattern '{:concepts ?all}
       :then    (fn [r] {:db #(db/concept-index-fetched % (get r 'all))})})

    :keyword
    (let [kw (:keyword db)]
      (when (and kw (not (contains? (:keyword-results db) kw)))
        {:pattern {:keywords {{:keyword kw} '?nodes}}
         :then    (fn [r] {:db #(db/keyword-fetched % kw (get r 'nodes))})}))

    nil))

(defn for-route
  "Pull operation needed after navigating to a parsed route."
  [{:keys [view]}]
  (case view
    :home     [:universe :learning :ideas]
    :cluster  [:universe :cluster :concept-graph :learning :ideas]
    :node     [:node :ideas]
    :keyword  :keyword
    :ask      :ask
    :concept  [:universe :concept :concept-graph :learning :ideas]
    :concepts :concept-index
    :study    :learning
    :explore  [:exploration :explorations]
    nil))

^:rct/test
(comment
  (def id #uuid "00000000-0000-0000-0000-000000000001")

  (= '{:sources ?sources :asks ?asks :universe ?u} (:pattern (resolve-pull :init db/initial-state))) ;=> true

  ;; asking: create the answer and search right away; nothing when not asking
  (:pattern (resolve-pull :ask-create (db/ask-submitted db/initial-state "Why?")))
  ;=> {:asks {nil {:ask/question "Why?"}}}
  (= '{:search {{:q "Why?"} ?hits}} (:pattern (resolve-pull :search (db/ask-submitted db/initial-state "Why?"))))
  ;=> true
  (resolve-pull :ask-create db/initial-state) ;=> nil

  ;; uncached node -> fetch it; cached -> nothing to do
  (= {:nodes {{:node/id id} '?node}}
     (:pattern (resolve-pull :node (db/open-node db/initial-state id))))
  ;=> true
  (resolve-pull :node (-> db/initial-state (db/node-fetched {:node/id id}) (db/open-node id))) ;=> nil

  ;; :then stores the node
  (let [db  (db/open-node db/initial-state id)
        eff ((:then (resolve-pull :node db)) {'node {:node/id id :node/title "T"}})]
    (:node/title (db/current-node ((:db eff) db))))
  ;=> "T"

  (= '{:keywords {{:keyword "confluence"} ?nodes}}
     (:pattern (resolve-pull :keyword (db/open-keyword db/initial-state "confluence"))))
  ;=> true

  (= {:concepts {{:concept/id id} '?concept}}
     (:pattern (resolve-pull :concept (db/open-concept db/initial-state id))))
  ;=> true
  (resolve-pull :concept-index (db/concept-index-fetched db/initial-state [])) ;=> nil

  (for-route {:view :node}) ;=> [:node :ideas]
  (for-route {:view :concepts}) ;=> :concept-index
  (for-route {:view :concept}) ;=> [:universe :concept :concept-graph :learning :ideas]
  (for-route {:view :cluster}) ;=> [:universe :cluster :concept-graph :learning :ideas]
  (for-route {:view :home}) ;=> [:universe :learning :ideas]
  ;; the map is fetched once, and again after scores moved
  (resolve-pull :universe (db/universe-fetched db/initial-state {:clusters [] :links []})) ;=> nil
  (some? (resolve-pull :universe (assoc-in (db/universe-fetched db/initial-state {:clusters [] :links []}) [:universe :stale?] true))) ;=> true
  (for-route {:view :study}) ;=> :learning
  (:pattern (resolve-pull :question-create (db/quiz-start db/initial-state [:concept id])))
  ;=> {:questions {nil {:question/item [:concept #uuid "00000000-0000-0000-0000-000000000001"]}}}
  ;; a book exercise of it
  (get-in (:pattern (resolve-pull :question-create (db/quiz-start db/initial-state [:concept id] id)))
          [:questions nil :question/exercise])
  ;=> #uuid "00000000-0000-0000-0000-000000000001"
  (resolve-pull :answer-create db/initial-state) ;=> nil
  (= '{:concept-graph ?g} (:pattern (resolve-pull :concept-graph db/initial-state))) ;=> true
  (for-route {:view :home})) ;=> nil
