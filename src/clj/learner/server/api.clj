(ns learner.server.api
  "Pull-pattern API (lasagna-remote), noun-only like flybot.sg's:

    '{:sources ?all}                               ; list sources
    '{:sources {{:source/id #uuid \"..\"} ?s}}      ; one source
    '{:nodes {{:node/id #uuid \"..\"} ?node}}       ; node + children, links, ancestors
    '{:keywords {{:keyword \"confluence\"} ?nodes}} ; nodes tagged with a keyword
    '{:search {{:q \"function extensionality\"} ?hits}} ; full-text + graph ranking, no model
    '{:asks ?recent}                                ; recent questions
    '{:asks {{:ask/id #uuid \"..\"} ?ask}}         ; an answer with its cited subgraph
    {:asks {nil {:ask/question \"..\"}}}           ; ask: answer with Claude, store, return it

  Personal app: no roles."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [learner.ask :as ask]
            [learner.db :as db]
            [sg.flybot.pullable.collection :as coll]))

(defrecord SourcesDataSource [conn]
  coll/DataSource
  (fetch [_ {:source/keys [id]}] (when id (db/source (d/db conn) id)))
  (list-all [_] (db/sources (d/db conn)))
  (create! [_ _] {:error {:type :forbidden :message "sources are created by ingest"}})
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ _] {:error {:type :forbidden}}))

(defn- ask-error [e]
  {:error {:type    (if (= :refusal (:stop-reason (ex-data e))) :refused :llm-error)
           :message (or (ex-message e) "The model call failed")}})

(defrecord AsksDataSource [conn client opts]
  coll/DataSource
  (fetch [_ {:ask/keys [id]}] (when (uuid? id) (ask/view (d/db conn) id)))
  (list-all [_] (ask/history (d/db conn) 20))
  (create! [_ {:ask/keys [question]}]
    (let [q (some-> question str/trim)]
      (if (or (str/blank? q) (> (count q) 2000))
        {:error {:type :invalid :message "Ask a question of up to 2000 characters"}}
        (try
          (ask/view (d/db conn) (ask/ask! conn @client q opts))
          (catch Exception e (ask-error e))))))
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ {:ask/keys [id]}]
    (when-let [eid (and (uuid? id) (d/entid (d/db conn) [:ask/id id]))]
      (d/transact! conn [[:db/retractEntity eid]])
      true)))

(defn- query-lookup
  "Non-enumerable, map-keyed, read-only resource: (get it query) -> (f query)."
  [f]
  (reify
    clojure.lang.ILookup
    (valAt [_ query] (f query))
    (valAt [this query not-found] (or (.valAt this query) not-found))
    coll/Wireable
    (->wire [_] nil)))

(defn make-api
  "api-fn for remote/wrap-api: ring request -> {:data .. :errors ..}.
  `client` is a delay of the Anthropic client (built on the first ask);
  `ask-opts` go to learner.ask/ask! (:models)."
  [conn client ask-opts]
  (let [asks    (coll/collection (->AsksDataSource conn client ask-opts) {:id-key :ask/id})
        search  (query-lookup (fn [{:keys [q]}] (when (and (string? q) (not (str/blank? q)))
                                                  (ask/search (d/db conn) q 12))))
        sources (coll/read-only (coll/collection (->SourcesDataSource conn)
                                                 {:id-key :source/id}))
        nodes   (query-lookup (fn [{:node/keys [id]}] (when (uuid? id) (db/node (d/db conn) id))))
        kws     (query-lookup (fn [{:keys [keyword]}] (when (string? keyword)
                                                        (db/nodes-with-keyword (d/db conn) keyword))))]
    (fn [_request]
      {:data   {:sources sources :nodes nodes :keywords kws :search search :asks asks}
       :errors {:detect :error
                :codes  {:forbidden 403 :not-found 404 :invalid 400 :refused 422 :llm-error 502}}})))
