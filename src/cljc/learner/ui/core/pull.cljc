(ns learner.ui.core.pull
  "Pull spec definitions: pattern + :then as data.

  Each named operation resolves to {:pattern ... :then ...}. :pattern is sent
  to /api (see learner.server.api); :then is (fn [response] effect-map).
  Pure data functions, testable on the JVM."
  (:require [learner.ui.core.db :as db]))

(defn resolve-pull
  "Resolve a named pull operation against db. nil when nothing needs fetching."
  [op db]
  (case op
    :init
    {:pattern '{:sources ?sources :asks ?asks}
     :then    (fn [r] {:db #(-> %
                                (db/sources-fetched (get r 'sources))
                                (db/history-fetched (get r 'asks)))})}

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
         :then    (fn [r] {:db #(db/node-fetched % (get r 'node))})}))

    :keyword
    (let [kw (:keyword db)]
      (when (and kw (not (contains? (:keyword-results db) kw)))
        {:pattern {:keywords {{:keyword kw} '?nodes}}
         :then    (fn [r] {:db #(db/keyword-fetched % kw (get r 'nodes))})}))

    nil))

(defn for-route
  "Pull operation needed after navigating to a parsed route."
  [{:keys [view]}]
  (case view :node :node :keyword :keyword :ask :ask nil))

^:rct/test
(comment
  (def id #uuid "00000000-0000-0000-0000-000000000001")

  (= '{:sources ?sources :asks ?asks} (:pattern (resolve-pull :init db/initial-state))) ;=> true

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

  (for-route {:view :node}) ;=> :node
  (for-route {:view :home})) ;=> nil
