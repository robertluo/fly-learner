(ns robertluo.fly-learner.server.ideas
  "Ideas, composed for the API: the ideas layer, its store and its anchors
  over the knowledge. App layer."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.ask :as ask]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ideas.knowledge :as anchors]
            [robertluo.fly-learner.ideas.store :as store]))

(defn- singular [term]
  (some #(when (str/ends-with? term %) (subs term 0 (- (count term) (count %)))) ["es" "s"]))

(defn refs
  "The concepts the `[[term]]` links in `text` name now: {term {:concept/id
  :concept/label :concept/gloss}}; a plural names the concept of its
  singular. A term naming nothing is left out (not a concept, yet)."
  [dbv text]
  (into {} (for [t (ideas/wikilinks text)
                 :let [c (or (concept/taken dbv [t]) (some->> (singular t) vector (concept/taken dbv)))]
                 :when c]
             [t (merge (select-keys c [:concept/id :concept/label])
                       (d/pull dbv [:concept/gloss] [:concept/id (:concept/id c)]))])))

(defn named
  "The passages whose text names `term` (a term of the learner's: a
  `[[..]]` link naming no concept), as node refs with their paths, by
  source and page; nil for no term."
  [dbv term]
  (when (and (string? term) (not (str/blank? term)) (<= (count term) 80))
    (->> (concept/naming-nodes dbv [term])
         (keep #(ask/ref-of dbv %))
         (sort-by (juxt (comp :source/title :node/source) #(or (:node/page %) 0) :node/level))
         (take 30)
         vec)))

(defn- resolved
  "`idea` with what it points at now: :idea/targets [{:key .. :kind .. :title ..} ..],
  :idea/within, the nodes containing the nodes it is on (so a section
  shows the ideas on anything inside it), and :idea/refs, the concepts its
  `[[..]]` links name (`refs`), and :idea/place: where its first passage
  is in its source, {:source/id :source/title :path [{:node/id :node/title :node/kind} ..]
  (root first, the passage last) :order [n ..] (reading order) :page},
  for arranging ideas as the sources go (a page quote: its page only)."
  [dbv a idea]
  (let [ks (ideas/targets a idea)]
    (assoc idea
           :idea/refs    (refs dbv (:idea/text idea))
           :idea/place   (some (fn [[kind x y]]
                                 (case kind
                                   :node (when-let [e (d/entid dbv [:node/id x])]
                                           (let [n    (d/pull dbv [:node/id :node/title :node/kind :node/order :node/page
                                                                   {:node/source [:source/id :source/title]}] e)
                                                 path (conj (db/ancestors-of dbv e) n)]
                                             {:source/id    (get-in n [:node/source :source/id])
                                              :source/title (get-in n [:node/source :source/title])
                                              :path  (mapv #(select-keys % [:node/id :node/title :node/kind]) path)
                                              :order (mapv #(or (:node/order %) 0) path)
                                              :page  (:node/page n)}))
                                   :page (when-let [t (:source/title (d/pull dbv [:source/title] [:source/id x]))]
                                           {:source/id x :source/title t :path [] :order [] :page y})
                                   nil))
                               (sort-by #(case (first %) :node 0 :page 1 2) ks))
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

(defn update!
  "Rewrite idea `id`: its `{:idea/stance :idea/text :idea/links}` (what it
  is about and when it was written stay); returns it resolved, or {:error ..}."
  [conn id {:idea/keys [stance text links]}]
  (let [db  (d/db conn)
        all (store/all db)
        old (some #(when (= id (:idea/id %)) %) all)
        ids (disj (set (map :idea/id all)) id)]
    (if-not old
      {:error {:type :not-found :message "No such idea"}}
      (let [i (ideas/idea {:stance stance :text text :anchors (:idea/anchors old) :links (filter ids links)}
                          id (:idea/at old))]
        (if (:error i)
          {:error {:type :invalid :message (:error i)}}
          (do (store/replace! conn i)
              (resolved db (anchors/anchors db) i)))))))

(defn delete! [conn id] (store/delete! conn id))

(defn focus
  "What a selected passage (a quote anchor) is in, for asking about it:
  {:quote text :nodes [node-id ..]}: its paragraph, or the paragraphs of the
  source page it was selected on. nil unless `anchor` is a quote."
  [db anchor]
  (when (and (map? anchor) (= :quote (:anchor/kind anchor)) (string? (:quote anchor)))
    (let [[kind a b] (ideas/target (anchors/anchors db) anchor)]
      {:quote (:quote anchor)
       :nodes (case kind
                :node [a]
                :page (vec (take 8 (sort-by str (db/nodes-on-page db a b))))
                [])})))
