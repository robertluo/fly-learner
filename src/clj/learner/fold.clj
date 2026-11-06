(ns learner.fold
  "Fold a flat list of source paragraphs into a multi-layered knowledge DAG.

  Bottom-up: paragraphs are leaves; consecutive nodes are grouped and
  summarised into a parent node, repeated until a single birdview node
  remains. Nodes sharing keywords are then cross-linked.

  Summarisation is a function argument so the folding logic can be tested
  without calling a model. See `llm-summarizer` for the real one.
  Reference: https://github.com/chenxiachan/textfold"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [jsonista.core :as json]
            [learner.llm :as llm]))

(def default-group-size 6)

(defn leaves [paragraphs]
  (mapv (fn [{:keys [page order text]}]
          {:node/id    (random-uuid)
           :node/title (let [t (str/trim text)] (subs t 0 (min 60 (count t))))
           :node/text  text
           :node/page  page
           :node/order order})
        paragraphs))

(defn- fold-level
  "Group `nodes` and summarise each group into a parent node."
  [summarize group-size nodes]
  (->> (partition-all group-size nodes)
       (map-indexed
        (fn [i group]
          (let [{:keys [title summary keywords]} (summarize group)]
            {:node/id       (random-uuid)
             :node/title    title
             :node/summary  summary
             :node/keywords (set keywords)
             :node/order    i
             :node/children (mapv :node/id group)})))
       vec))

(defn fold
  "Return all nodes of the DAG with :node/level assigned (0 = birdview).
  `summarize` : [nodes] -> {:title .. :summary .. :keywords [..]}"
  ([summarize paragraphs] (fold summarize paragraphs {}))
  ([summarize paragraphs {:keys [group-size] :or {group-size default-group-size}}]
   (let [layers (loop [layers [(leaves paragraphs)]]
                  (if (<= (count (peek layers)) 1)
                    layers
                    (recur (conj layers (fold-level summarize group-size (peek layers))))))
         depth  (dec (count layers))]
     (vec (mapcat (fn [i layer] (map #(assoc % :node/level (- depth i)) layer))
                  (range) layers)))))

(defn link-by-keywords
  "Add :node/links between non-leaf nodes on the same level sharing keywords."
  [nodes]
  (let [by-level (group-by :node/level (filter :node/keywords nodes))
        links    (into {}
                       (for [[_ ns] by-level, n ns
                             :let [ls (->> ns
                                           (remove #(= (:node/id %) (:node/id n)))
                                           (filter #(seq (set/intersection (:node/keywords n)
                                                                           (:node/keywords %))))
                                           (mapv :node/id))]
                             :when (seq ls)]
                         [(:node/id n) ls]))]
    (mapv #(if-let [ls (links (:node/id %))] (assoc % :node/links ls) %) nodes)))

(defn ->tx
  "Transaction data for a source and its folded `nodes`.
  Node uuids double as tempids so children/links resolve in one transaction."
  [{:keys [title path type] :or {type :pdf}} nodes]
  (let [tid    str
        source {:db/id "source" :source/id (random-uuid)
                :source/type type :source/title title :source/path path}
        root   (first (filter #(zero? (:node/level %)) nodes))]
    (into [(cond-> source root (assoc :source/root (tid (:node/id root))))]
          (map (fn [n]
                 (cond-> (assoc n :db/id (tid (:node/id n)) :node/source "source")
                   (:node/children n) (update :node/children #(mapv tid %))
                   (:node/links n)    (update :node/links #(mapv tid %)))))
          nodes)))

;; --- LLM summariser -------------------------------------------------------

(def ^:private system-prompt
  "You fold source text into a layered study map. Given consecutive passages,
write a short title, a summary that preserves the key ideas, and 3-8
keywords (lowercase noun phrases) that may also appear elsewhere in the
source. Reply with only a JSON object: {\"title\": .., \"summary\": .., \"keywords\": [..]}")

(defn llm-summarizer [client]
  (fn [nodes]
    (let [passages (->> nodes
                        (map #(or (:node/summary %) (:node/text %)))
                        (map-indexed #(str "[" (inc %1) "] " %2))
                        (str/join "\n\n"))]
      (-> (llm/complete client {:system system-prompt :prompt passages :effort :medium})
          (json/read-value json/keyword-keys-object-mapper)))))
