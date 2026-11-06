(ns robertluo.fly-learner.server.ideas
  "Ideas, composed for the API: the ideas layer, its store and its anchors
  over the knowledge. App layer."
  (:require [datalevin.core :as d]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ideas.knowledge :as anchors]
            [robertluo.fly-learner.ideas.store :as store]))

(defn- resolved
  "`idea` with what it points at now: :idea/targets [{:key .. :kind .. :title ..} ..],
  and :idea/within, the nodes containing the nodes it is on (so a section
  shows the ideas on anything inside it)."
  [dbv a idea]
  (let [ks (ideas/targets a idea)]
    (assoc idea
           :idea/targets (vec (for [k ks] (assoc (ideas/describe a k) :key k)))
           :idea/within  (vec (distinct (for [[kind id] ks :when (= :node kind)
                                              :let [e (d/entid dbv [:node/id id])] :when e
                                              anc (db/ancestors-of dbv e)]
                                          (:node/id anc)))))))

(defn all
  "Every idea, resolved, oldest first; links to deleted ideas dropped."
  [db]
  (let [a   (anchors/anchors db)
        all (store/all db)
        ids (set (map :idea/id all))]
    (mapv #(->> (update % :idea/links (partial filterv ids)) (resolved db a)) all)))

(defn create!
  "Store a new idea from the learner's `{:idea/stance :idea/text :idea/anchors
  :idea/links}`; returns it resolved, or {:error ..}."
  [conn {:idea/keys [stance text anchors links]}]
  (let [db  (d/db conn)
        ids (set (map :idea/id (store/all db)))
        i   (ideas/idea {:stance stance :text text :anchors anchors :links (filter ids links)}
                        (random-uuid) (System/currentTimeMillis))]
    (if (:error i)
      {:error {:type :invalid :message (:error i)}}
      (let [a (anchors/anchors db)]
        (if (empty? (ideas/targets a i))
          {:error {:type :invalid :message "What it is about is not in the knowledge base"}}
          (do (store/save! conn i)
              (resolved db a i)))))))

(defn delete! [conn id] (store/delete! conn id))

(defn focus
  "What a selected passage (a quote anchor) is in, for asking about it:
  {:quote text :nodes [node-id ..]}: its paragraph, or the paragraphs of the
  PDF page it was selected on. nil unless `anchor` is a quote."
  [db anchor]
  (when (and (map? anchor) (= :quote (:anchor/kind anchor)) (string? (:quote anchor)))
    (let [[kind a b] (ideas/target (anchors/anchors db) anchor)]
      {:quote (:quote anchor)
       :nodes (case kind
                :node [a]
                :page (vec (take 8 (sort-by str (db/nodes-on-page db a b))))
                [])})))
