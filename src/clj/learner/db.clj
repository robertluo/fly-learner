(ns learner.db
  "Datalevin storage for sources, the folded knowledge DAG, questions and
  the learner's progress."
  (:require [datalevin.core :as d]))

(def schema
  {;; sources -----------------------------------------------------------
   :source/id       {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :source/type     {:db/valueType :db.type/keyword} ; :pdf, later :web, :epub ...
   :source/title    {:db/valueType :db.type/string}
   :source/path     {:db/valueType :db.type/string}
   :source/root     {:db/valueType :db.type/ref}     ; level-0 birdview node

   ;; knowledge nodes ---------------------------------------------------
   :node/id         {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :node/source     {:db/valueType :db.type/ref}
   :node/level      {:db/valueType :db.type/long}
   :node/title      {:db/valueType :db.type/string}
   :node/summary    {:db/valueType :db.type/string :db/fulltext true}
   :node/text       {:db/valueType :db.type/string :db/fulltext true}
   :node/keywords   {:db/valueType :db.type/string :db/cardinality :db.cardinality/many}
   :node/page       {:db/valueType :db.type/long}
   :node/order      {:db/valueType :db.type/long}
   :node/children   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :node/links      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}

   ;; questions ---------------------------------------------------------
   :question/id     {:db/valueType :db.type/uuid :db/unique :db.unique/identity}
   :question/node   {:db/valueType :db.type/ref}
   :question/kind   {:db/valueType :db.type/keyword}
   :question/prompt {:db/valueType :db.type/string}
   :question/answer {:db/valueType :db.type/string}

   ;; progress ----------------------------------------------------------
   :attempt/question {:db/valueType :db.type/ref}
   :attempt/response {:db/valueType :db.type/string}
   :attempt/correct? {:db/valueType :db.type/boolean}
   :attempt/at       {:db/valueType :db.type/instant}

   ;; spaced-repetition state, one per node the learner is studying
   :review/node     {:db/valueType :db.type/ref :db/unique :db.unique/identity}
   :review/ease     {:db/valueType :db.type/double}
   :review/interval {:db/valueType :db.type/long}   ; days
   :review/due      {:db/valueType :db.type/instant}})

(defn connect
  "Open (or create) the database at `dir`."
  [dir]
  (d/get-conn dir schema))

(defn close [conn]
  (d/close conn))

(defn node
  "Pull a node with its direct children and links (ids + titles only)."
  [db node-id]
  (d/pull db '[* {:node/children [:node/id :node/title :node/level :node/order]}
               {:node/links [:node/id :node/title :node/level]}]
          [:node/id node-id]))

(defn sources [db]
  (d/q '[:find [(pull ?s [:source/id :source/title :source/type
                          {:source/root [:node/id]}]) ...]
         :where [?s :source/id]]
       db))
