(ns learner.server.api
  "Pull-pattern API (lasagna-remote), noun-only like flybot.sg's:

    '{:sources ?all}                               ; list sources
    '{:sources {{:source/id #uuid \"..\"} ?s}}      ; one source
    '{:nodes {{:node/id #uuid \"..\"} ?node}}       ; node + children, links, ancestors
    '{:keywords {{:keyword \"confluence\"} ?nodes}} ; nodes tagged with a keyword

  Everything is read-only for now. Personal app: no roles."
  (:require [datalevin.core :as d]
            [learner.db :as db]
            [sg.flybot.pullable.collection :as coll]))

(defrecord SourcesDataSource [conn]
  coll/DataSource
  (fetch [_ {:source/keys [id]}] (when id (db/source (d/db conn) id)))
  (list-all [_] (db/sources (d/db conn)))
  (create! [_ _] {:error {:type :forbidden :message "sources are created by ingest"}})
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ _] {:error {:type :forbidden}}))

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
  "api-fn for remote/wrap-api: ring request -> {:data .. :errors ..}."
  [conn]
  (let [sources (coll/read-only (coll/collection (->SourcesDataSource conn)
                                                 {:id-key :source/id}))
        nodes   (query-lookup (fn [{:node/keys [id]}] (when (uuid? id) (db/node (d/db conn) id))))
        kws     (query-lookup (fn [{:keys [keyword]}] (when (string? keyword)
                                                        (db/nodes-with-keyword (d/db conn) keyword))))]
    (fn [_request]
      {:data   {:sources sources :nodes nodes :keywords kws}
       :errors {:detect :error
                :codes  {:forbidden 403 :not-found 404}}})))
