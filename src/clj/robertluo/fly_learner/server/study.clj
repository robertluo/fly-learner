(ns robertluo.fly-learner.server.study
  "Studying, composed for the API: learning (with the model and curriculum
  picked in robertluo.fly-learner.server.system), its evidence store, and
  quiz. App layer: the only place these meet.

  `learning` below is {:model Model :curriculum (fn [db] Curriculum)}."
  (:require [datalevin.core :as d]
            [robertluo.fly-learner.concept.cluster :as cluster]
            [robertluo.fly-learner.concept.domain :as domain]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.learning.concepts :as learning.concepts]
            [robertluo.fly-learner.learning.store :as store]
            [robertluo.fly-learner.quiz :as quiz]))

(def next-limit 6)

(defn- now [] (System/currentTimeMillis))

(defn- scored
  "Curriculum and scores for `db` at `at`."
  [db {:keys [model curriculum]} at]
  (let [c (curriculum db)]
    [c (learning/scores model c (learning/states model (store/evidence db)) at)]))

(defn- item-ref [c scores item]
  (let [{:keys [title about]} (learning/material c item)]
    {:item item :title title :about about :score (get scores item 0)}))

(defn- next-refs
  "What to study next in `scope` (part of curriculum `c`). An item outside
  the scope is a prerequisite items in it rest on: :prerequisite? true."
  [c scope scores]
  (let [in? (learning/items scope)]
    (vec (for [[item p] (learning/next-items scope scores next-limit)]
           (cond-> (assoc (item-ref c scores item) :priority p)
             (not (contains? in? item)) (assoc :prerequisite? true))))))

(defn cluster-scope [id] [:cluster id])

(defn- goal-clusters
  "Ids of the clusters the learner is learning now (that still exist)."
  [db]
  (let [live (set (map :cluster/id (cluster/stored db)))]
    (vec (keep (fn [[kind id]] (when (and (= :cluster kind) (live id)) id)) (store/goals db)))))

(defn- focus
  "`c` limited to the items of the clusters `ids` (all of `c` when none)."
  [db c ids]
  (if (empty? ids)
    c
    (let [members (into #{} (comp (filter (comp (set ids) :cluster/id)) (mapcat :members)) (cluster/stored db))]
      (learning/restricted c (learning.concepts/concepts-items members (keys (learning/items c)))))))

(defn overview
  "What the learner knows now: {:overall :scores {item score} (only items
  above 0) :next [{:item :title :about :score :priority :prerequisite?} ..]
  (:prerequisite? when outside the clusters, see `next-refs`) :answers n
  :goals [cluster-id ..] :exercises {question-id best grade}} (the authors'
  exercises answered). :next comes from the clusters being learned, if
  any; :overall is always the whole graph's."
  [db learning]
  (let [[c scores] (scored db learning (now))
        goals      (goal-clusters db)
        evidence   (store/evidence db)]
    {:overall (learning/overall c scores)
     :scores  (into {} (filter (comp pos? val)) scores)
     :next    (next-refs c (focus db c goals) scores)
     :answers   (count (distinct (keep #(when (= :answer (:kind %)) [(:source %) (:at %)]) evidence)))
     :goals     goals
     :exercises (quiz/attempts db evidence)}))

(defn universe
  "Every cluster with how well it is known and whether it is being learned:
  robertluo.fly-learner.concept.cluster/universe, each cluster with :score
  and :learning?, plus :domains (concept.domain/named: the map's regions,
  overlapping), :goals [cluster-id ..] and :overall."
  [db learning]
  (let [[c scores] (scored db learning (now))
        goals (set (goal-clusters db))
        all   (keys (learning/items c))
        u     (cluster/universe db)]
    (-> u
        (update :clusters (fn [cs]
                            (mapv (fn [{:keys [concepts] :cluster/keys [id] :as cl}]
                                    (let [its (learning.concepts/concepts-items (set concepts) all)]
                                      (assoc cl
                                             :score (learning/overall (learning/restricted c its) scores)
                                             :learning? (contains? goals id))))
                                  cs)))
        (assoc :domains (domain/named db u)
               :goals (vec (goal-clusters db)) :overall (learning/overall c scores)))))

(defonce ^:private naming (atom {:running? false :failed-at 0}))

(def ^:private naming-retry-ms
  "After the model failed to name domains (no key, say), wait this long before trying again."
  (* 10 60 1000))

(defn name-domains-later!
  "Name the domains of universe `u` that have no name yet, in the background
  (the :strong model, `client` a ref to it): one run at a time, and not again
  soon after a failure. The map shows them unnamed meanwhile."
  [conn client opts u]
  (when (and (some (complement :domain/id) (:domains u))
             (let [{:keys [running? failed-at]} @naming]
               (and (not running?) (> (now) (+ failed-at naming-retry-ms))))
             (compare-and-set! naming @naming (assoc @naming :running? true)))
    (future
      (try (domain/name! conn u (domain/llm-namer @client opts))
           (swap! naming assoc :running? false)
           (catch Throwable e
             (println "Naming the map's domains failed:" (ex-message e))
             (swap! naming assoc :running? false :failed-at (now)))))
    nil))

(defn cluster
  "Cluster `id` for its page: {:cluster/id :next [item ..]} where :next is
  what to study next within it; nil when there is no such cluster."
  [db learning id]
  (when (some #(= id (:cluster/id %)) (cluster/stored db))
    (let [[c scores] (scored db learning (now))]
      {:cluster/id id
       :next       (next-refs c (focus db c [id]) scores)})))

(defn learn!
  "Learn cluster `id` now (`on?`), or stop. Returns the overview."
  [conn learning id on?]
  (store/goal! conn (cluster-scope id) on?)
  (overview (d/db conn) learning))

(defn clusters-replaced!
  "After clustering again: learning a cluster that is gone becomes learning
  its successor ({old-id new-id}, from cluster/refresh!). Returns the ids
  of the clusters whose goals moved."
  [conn successors]
  (vec (for [[old new] successors
             :when (store/move-goal! conn (cluster-scope old) (cluster-scope new))]
         old)))

(defn- public-question
  "A question as the learner sees it before answering: no model answer."
  [c scores q]
  (merge (select-keys q [:question/id :question/item :question/prompt :question/by :question/title
                         :question/weight :question/anchor :question/context :question/verbatim?])
         (item-ref c scores (:question/item q))))

(defn new-question!
  "A question on `item`, or on the item to study next when nil: `exercise`
  (a question id) when one of its authored ones, else the first authored
  one not answered yet, else one the model writes. Saved, and returned
  without its answer, or {:error ..}."
  [conn client learning {:keys [item exercise]} opts]
  (let [db (d/db conn)
        [c scores] (scored db learning (now))
        item (or item (ffirst (learning/next-items c scores 1)))]
    (if-not (contains? (learning/items c) item)
      {:error {:type :invalid :message "Nothing to study there"}}
      (let [authored (quiz/authored c item)
            chosen   (when exercise (first (filter #(= exercise (:question/id %)) authored)))]
        (if (and exercise (not chosen))
          {:error {:type :not-found :message "No such exercise on this"}}
          (let [q (or chosen
                      (first (remove #(contains? (quiz/attempts db (store/evidence db)) (:question/id %)) authored))
                      (first (quiz/generate client c item opts)))]
            (quiz/save! conn [q])
            (public-question c scores q)))))))

(defn answer!
  "Grade `response` to question `id`, record the evidence, and return the
  grading with the model answer, key points and the score of the item and
  of the whole curriculum before and after."
  [conn client learning id response opts]
  (if-let [q (quiz/question (d/db conn) id)]
    (let [item   (:question/item q)
          [c s0] (scored (d/db conn) learning (now))
          graded (quiz/grade client q response opts)
          _      (store/record! conn (quiz/evidence q graded (now)))
          db     (d/db conn)
          [_ s1] (scored db learning (now))]
      (merge graded
             (select-keys q [:question/id :question/answer :question/points :question/by :question/weight])
             {:before   {:item (get s0 item 0) :overall (learning/overall c s0)}
              :after    {:item (get s1 item 0) :overall (learning/overall c s1)}
              :overview (overview db learning)}))
    {:error {:type :not-found :message "No such question"}}))
