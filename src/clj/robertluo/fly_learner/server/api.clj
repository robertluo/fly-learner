(ns robertluo.fly-learner.server.api
  "Pull-pattern API (lasagna-remote), noun-only like flybot.sg's:

    '{:sources ?all}                               ; list sources
    '{:sources {{:source/id #uuid \"..\"} ?s}}      ; one source
    '{:nodes {{:node/id #uuid \"..\"} ?node}}       ; node + children, links, ancestors
    '{:keywords {{:keyword \"confluence\"} ?nodes}} ; nodes tagged with a keyword
    '{:concepts ?all}                              ; every concept (refs, by label)
    '{:concepts {{:concept/id #uuid \"..\"} ?c}}   ; a concept: hierarchy, related, mentions
    '{:concept-graph ?g}                           ; every concept and edge, explored client-side
    '{:learning ?l}                                ; what the learner knows: overall, scores, next
    '{:ideas ?all}                                 ; the learner's ideas, resolved to what they are about
    {:ideas {nil {:idea/stance :note ..}}}         ; add one; {:ideas {{:idea/id id} nil}} removes it
    {:questions {nil {:question/item item}}}       ; write a question (item nil: what to study next)
    {:answers {nil {:question/id .. :response \"..\"}}} ; grade an answer, record it, new scores
    '{:search {{:q \"function extensionality\"} ?hits}} ; full-text + graph ranking, no model
    '{:asks ?recent}                                ; recent questions
    '{:asks {{:ask/id #uuid \"..\"} ?ask}}         ; an answer with its cited subgraph
    {:asks {nil {:ask/question \"..\"}}}           ; ask: answer with Claude, store, return it
    {:asks {nil {:ask/question \"..\" :ask/anchor quote}}} ; ask about a selected passage (a quote anchor)

  Personal app: no roles."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.ask :as ask]
            [robertluo.fly-learner.concept.view :as concept-view]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.server.ideas :as ideas]
            [robertluo.fly-learner.server.study :as study]
            [sg.flybot.pullable.collection :as coll]))

(defrecord SourcesDataSource [conn]
  coll/DataSource
  (fetch [_ {:source/keys [id]}] (when id (db/source (d/db conn) id)))
  (list-all [_] (db/sources (d/db conn)))
  (create! [_ _] {:error {:type :forbidden :message "sources are created by ingest"}})
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ _] {:error {:type :forbidden}}))

(defrecord ConceptsDataSource [conn]
  coll/DataSource
  (fetch [_ {:concept/keys [id]}] (when (uuid? id) (concept-view/concept (d/db conn) id)))
  (list-all [_] (concept-view/index (d/db conn)))
  (create! [_ _] {:error {:type :forbidden :message "concepts are made by resolution and curation"}})
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ _] {:error {:type :forbidden}}))

(defn- ask-error [e]
  {:error {:type    (if (= :refusal (:stop-reason (ex-data e))) :refused :llm-error)
           :message (or (ex-message e) "The model call failed")}})

(defrecord AsksDataSource [conn client opts]
  coll/DataSource
  (fetch [_ {:ask/keys [id]}] (when (uuid? id) (ask/view (d/db conn) id)))
  (list-all [_] (ask/history (d/db conn) 20))
  (create! [_ {:ask/keys [question anchor]}]
    (let [q     (some-> question str/trim)
          focus (when anchor (ideas/focus (d/db conn) anchor))]
      (cond
        (or (str/blank? q) (> (count q) 2000))
        {:error {:type :invalid :message "Ask a question of up to 2000 characters"}}
        (and anchor (not focus))
        {:error {:type :invalid :message "Ask about a selected passage"}}
        :else
        (try
          (ask/view (d/db conn) (ask/ask! conn @client q (cond-> opts focus (assoc :focus focus))))
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

(defrecord QuestionsDataSource [conn client learning opts]
  coll/DataSource
  (fetch [_ _] nil)
  (list-all [_] [])
  (create! [_ {:question/keys [item]}]
    (try (study/new-question! conn @client learning item opts)
         (catch Exception e (ask-error e))))
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ _] {:error {:type :forbidden}}))

(defrecord AnswersDataSource [conn client learning opts]
  coll/DataSource
  (fetch [_ _] nil)
  (list-all [_] [])
  (create! [_ {:question/keys [id] :keys [response]}]
    (cond
      (not (uuid? id)) {:error {:type :invalid :message "Which question?"}}
      (not (string? response)) {:error {:type :invalid :message "Answer in words"}}
      (> (count response) 5000) {:error {:type :invalid :message "Answer in up to 5000 characters"}}
      :else (try (study/answer! conn @client learning id response opts)
                 (catch Exception e (ask-error e)))))
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ _] {:error {:type :forbidden}}))

(defrecord IdeasDataSource [conn]
  coll/DataSource
  (fetch [_ {:idea/keys [id]}] (when (uuid? id) (some #(when (= id (:idea/id %)) %) (ideas/all (d/db conn)))))
  (list-all [_] (ideas/all (d/db conn)))
  (create! [_ data] (ideas/create! conn data))
  (update! [_ _ _] {:error {:type :forbidden :message "ideas are added or removed"}})
  (delete! [_ {:idea/keys [id]}] (boolean (and (uuid? id) (ideas/delete! conn id)))))

(defn- lazy-value
  "A value computed by `f` only when a pattern binds it (serialised via ->wire)."
  [f]
  (reify coll/Wireable (->wire [_] (f))))

(defn make-api
  "api-fn for remote/wrap-api: ring request -> {:data .. :errors ..}.
  `client` is a delay of the Anthropic client (built on the first ask);
  `ask-opts` go to robertluo.fly-learner.ask/ask! (:models), and to quiz;
  `learning` is {:model .. :curriculum (fn [db] ..)} (see server.system)."
  [conn client ask-opts learning]
  (let [asks    (coll/collection (->AsksDataSource conn client ask-opts) {:id-key :ask/id})
        search  (query-lookup (fn [{:keys [q]}] (when (and (string? q) (not (str/blank? q)))
                                                  (ask/search (d/db conn) q 12))))
        sources (coll/read-only (coll/collection (->SourcesDataSource conn)
                                                 {:id-key :source/id}))
        concepts (coll/read-only (coll/collection (->ConceptsDataSource conn)
                                                  {:id-key :concept/id}))
        questions (coll/collection (->QuestionsDataSource conn client learning ask-opts) {:id-key :question/id})
        answers   (coll/collection (->AnswersDataSource conn client learning ask-opts) {:id-key :question/id})
        idea-coll (coll/collection (->IdeasDataSource conn) {:id-key :idea/id})
        nodes   (query-lookup (fn [{:node/keys [id]}] (when (uuid? id) (db/node (d/db conn) id))))
        kws     (query-lookup (fn [{:keys [keyword]}] (when (string? keyword)
                                                        (db/nodes-with-keyword (d/db conn) keyword))))]
    (fn [_request]
      {:data   {:sources sources :nodes nodes :keywords kws :search search :asks asks
                :concepts concepts
                :concept-graph (lazy-value #(concept-view/graph (d/db conn)))
                :learning (lazy-value #(study/overview (d/db conn) learning))
                :questions questions :answers answers
                :ideas idea-coll}
       :errors {:detect :error
                :codes  {:forbidden 403 :not-found 404 :invalid 400 :refused 422 :llm-error 502}}})))
