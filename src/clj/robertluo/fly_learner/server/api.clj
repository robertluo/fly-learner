(ns robertluo.fly-learner.server.api
  "Pull-pattern API (lasagna-remote), noun-only like flybot.sg's:

    '{:sources ?all}                               ; list sources
    '{:sources {{:source/id #uuid \"..\"} ?s}}      ; one source
    '{:nodes {{:node/id #uuid \"..\"} ?node}}       ; node + children, links, ancestors
    '{:keywords {{:keyword \"confluence\"} ?nodes}} ; nodes tagged with a keyword
    '{:concepts ?all}                              ; every concept (refs, by label)
    '{:concepts {{:concept/id #uuid \"..\"} ?c}}   ; a concept: hierarchy, related, mentions
    {:concepts {nil {:concept/label \"..\" :concept/gloss \"..\" :concept/aliases [..]}}} ; add one the model
                                                    ; missed (tagged on the passages naming it)
    {:concepts {{:concept/id id} nil}}             ; not a concept: remove it, its names ignored
    '{:concept-graph ?g}                           ; every concept and edge, explored client-side
    '{:learning ?l}                                ; what the learner knows: overall, scores, next, goals
    '{:universe ?u}                                ; every cluster (topic) with its score; links between them
    {:goals {nil {:cluster/id id}}}                ; learn a cluster now; {:goals {{:cluster/id id} nil}} stops
    '{:clusters {{:cluster/id #uuid \"..\"} ?c}}   ; a cluster's page: what to study next in it
    {:clusters {{:cluster/id id} nil}}             ; not a topic: remove it and its concepts
    '{:ideas ?all}                                 ; the learner's ideas, resolved to what they are about
    {:ideas {nil {:idea/stance :note ..}}}         ; add one; {:ideas {{:idea/id id} nil}} removes it
    {:questions {nil {:question/item item}}}       ; a question (item nil: what to study next): the
                                                    ; author's exercise not answered yet, else written
    {:questions {nil {:question/item item :question/exercise id}}} ; that exercise of the item
    {:answers {nil {:question/id .. :response \"..\"}}} ; grade an answer, record it, new scores
    '{:search {{:q \"function extensionality\"} ?hits}} ; full-text + graph ranking, no model
    '{:goto {{:q \"nash\"} ?r}}                    ; {:concepts [..] :exercises [..]} the box names, as typed
    '{:asks ?recent}                                ; recent questions
    '{:asks {{:ask/id #uuid \"..\"} ?ask}}         ; an answer with its cited subgraph
    {:asks {nil {:ask/question \"..\"}}}           ; ask: answer with Claude, store, return it
    {:asks {nil {:ask/question \"..\" :ask/anchor quote}}} ; ask about a selected passage (a quote anchor)
    '{:explorations ?recent}                        ; explorations kept, newest first
    '{:explorations {{:exploration/id #uuid \"..\"} ?x}} ; one: its spec, node and cited nodes
    {:explorations {nil {:exploration/node id}}}   ; make one of a node with Claude (its proposal's pitch)
    {:explorations {nil {:exploration/previous id :exploration/feedback \"..\"}}} ; remake one
  A node (:nodes) carries :node/explore: its proposal, if any, and its explorations.
    '{:keypoints {{:source/id #uuid \"..\"} ?k}}  ; a source's key sentences [{:page :para :node :quote :label}]
    '{:pages {{:source/id #uuid \"..\" :page 40 :near #uuid \"..\"} ?n}} ; the node (as :nodes) to show beside a page

  Personal app: no roles."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.ask :as ask]
            [robertluo.fly-learner.concept.view :as concept-view]
            [robertluo.fly-learner.exercises :as exercises]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.explore.knowledge :as explore]
            [robertluo.fly-learner.explore.store :as explore.store]
            [robertluo.fly-learner.keypoints :as keypoints]
            [robertluo.fly-learner.server.curation :as curation]
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

(defn- concept-page
  "Concept `id` for its page (concept.view/concept) with the exercises the
  author wrote testing it (:concept/exercises: id, label, summary, page,
  :node/id, :source/id)."
  [db id]
  (some-> (concept-view/concept db id)
          (assoc :concept/exercises (mapv #(select-keys % [:exercise/id :exercise/label :exercise/summary
                                                           :exercise/page :node/id :source/id])
                                          (exercises/of-concept db id)))))

(defn- ask-error [e]
  {:error {:type    (if (= :refusal (:stop-reason (ex-data e))) :refused :llm-error)
           :message (or (ex-message e) "The model call failed")}})

(defrecord ConceptsDataSource [conn client opts]
  coll/DataSource
  (fetch [_ {:concept/keys [id]}] (when (uuid? id) (concept-page (d/db conn) id)))
  (list-all [_] (concept-view/index (d/db conn)))
  (create! [_ {:concept/keys [label gloss aliases]}]
    (if (and (string? label) (or (nil? gloss) (string? gloss))
             (or (nil? aliases) (and (sequential? aliases) (every? string? aliases))))
      (try
        (let [r (curation/add-concept! conn client opts {:concept/label label :concept/gloss gloss
                                                         :concept/aliases aliases})]
          (if (:error r) r (assoc (concept-page (d/db conn) (:concept/id r)) :mentions (:mentions r))))
        (catch Exception e (ask-error e)))
      {:error {:type :invalid :message "A concept is a name, maybe a gloss and other names"}}))
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ {:concept/keys [id]}]
    (boolean (and (uuid? id) (curation/remove-concept! conn id)))))

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

(defrecord ExplorationsDataSource [conn client opts]
  coll/DataSource
  (fetch [_ {:exploration/keys [id]}] (when (uuid? id) (explore/exploration (d/db conn) id)))
  (list-all [_] (explore/all (d/db conn)))
  (create! [_ {:exploration/keys [node previous feedback]}]
    (let [db   (d/db conn)
          prev (when (uuid? previous) (explore/exploration db previous))
          node (or node (get-in prev [:exploration/node :node/id]))]
      (cond
        (and previous (not prev)) {:error {:type :not-found :message "No such exploration"}}
        (not (and (uuid? node) (d/entid db [:node/id node])))
        {:error {:type :invalid :message "Explore which passage?"}}
        (and feedback (or (not (string? feedback)) (> (count feedback) 2000)))
        {:error {:type :invalid :message "Ask in up to 2000 characters"}}
        :else
        (try
          (let [r (explore/make! conn @client node (assoc opts :previous (when prev previous) :feedback feedback))]
            (if-let [id (:exploration/id r)]
              (explore/exploration (d/db conn) id)
              {:error {:type :llm-error :message (:error r)}}))
          (catch Exception e (ask-error e))))))
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ {:exploration/keys [id]}]
    (boolean (and (uuid? id) (explore.store/delete! conn id)))))

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
  (create! [_ {:question/keys [item exercise]}]
    (try (study/new-question! conn @client learning {:item item :exercise exercise} opts)
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

(defrecord ClustersDataSource [conn learning]
  coll/DataSource
  (fetch [_ {:cluster/keys [id]}] (when (uuid? id) (study/cluster (d/db conn) learning id)))
  (list-all [_] [])
  (create! [_ _] {:error {:type :forbidden :message "clusters are made from the concept graph"}})
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ {:cluster/keys [id]}]
    (boolean (and (uuid? id) (curation/remove-topic! conn id)))))

(defrecord GoalsDataSource [conn learning]
  coll/DataSource
  (fetch [_ _] nil)
  (list-all [_] [])
  (create! [_ {:cluster/keys [id]}]
    (if (uuid? id)
      {:cluster/id id :overview (study/learn! conn learning id true)}
      {:error {:type :invalid :message "Which cluster?"}}))
  (update! [_ _ _] {:error {:type :forbidden}})
  (delete! [_ {:cluster/keys [id]}]
    (when (uuid? id) (study/learn! conn learning id false) true)))

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
        goto    (query-lookup (fn [{:keys [q]}] (when (string? q)
                                                  (let [db (d/db conn)]
                                                    {:concepts  (or (concept-view/lookup db q 8) [])
                                                     :exercises (or (exercises/lookup db q 6) [])}))))
        sources (coll/read-only (coll/collection (->SourcesDataSource conn)
                                                 {:id-key :source/id}))
        concepts (coll/collection (->ConceptsDataSource conn client ask-opts) {:id-key :concept/id})
        questions (coll/collection (->QuestionsDataSource conn client learning ask-opts) {:id-key :question/id})
        answers   (coll/collection (->AnswersDataSource conn client learning ask-opts) {:id-key :question/id})
        idea-coll (coll/collection (->IdeasDataSource conn) {:id-key :idea/id})
        explorations (coll/collection (->ExplorationsDataSource conn client ask-opts) {:id-key :exploration/id})
        goals     (coll/collection (->GoalsDataSource conn learning) {:id-key :cluster/id})
        clusters  (coll/collection (->ClustersDataSource conn learning) {:id-key :cluster/id})
        node-of (fn [db id] (some-> (db/node db id) (assoc :node/explore (explore/for-node db id))))
        nodes   (query-lookup (fn [{:node/keys [id]}] (when (uuid? id) (node-of (d/db conn) id))))
        pages   (query-lookup (fn [{:keys [page near] :source/keys [id]}]
                                (when (and (uuid? id) (uuid? near) (pos-int? page))
                                  (let [db (d/db conn)]
                                    (some->> (db/node-at-page db id page near) (node-of db))))))
        kps     (query-lookup (fn [{:source/keys [id]}] (when (uuid? id) (keypoints/for-source (d/db conn) id))))
        kws     (query-lookup (fn [{:keys [keyword]}] (when (string? keyword)
                                                        (db/nodes-with-keyword (d/db conn) keyword))))]
    (fn [_request]
      {:data   {:sources sources :nodes nodes :pages pages :keypoints kps :keywords kws :search search :goto goto :asks asks
                :concepts concepts
                :concept-graph (lazy-value #(concept-view/graph (d/db conn)))
                :learning (lazy-value #(study/overview (d/db conn) learning))
                :universe (lazy-value #(study/universe (d/db conn) learning))
                :goals goals
                :clusters clusters
                :questions questions :answers answers
                :ideas idea-coll
                :explorations explorations}
       :errors {:detect :error
                :codes  {:forbidden 403 :not-found 404 :invalid 400 :refused 422 :llm-error 502}}})))
