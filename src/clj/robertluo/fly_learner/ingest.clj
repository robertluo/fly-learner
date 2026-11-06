(ns robertluo.fly-learner.ingest
  "Source -> knowledge DAG -> database."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [datalevin.core :as d]
            [robertluo.fly-learner.fold :as fold]
            [robertluo.fly-learner.ingest.outline :as outline]
            [robertluo.fly-learner.ingest.pdf :as pdf]))

(defn outline-pdf
  "Extract `path` into a regrouped section tree (no model calls)."
  [path]
  (let [{:keys [title blocks]} (pdf/extract path)]
    (fold/regroup (outline/tree title blocks))))

(defn- internal-count [tree]
  (if (:children tree) (inc (reduce + (map internal-count (:children tree)))) 0))

(defn fold-tree
  "Summarise a section tree with `summarize` and return linked nodes."
  [summarize tree]
  (-> (fold/summarize-tree summarize tree)
      fold/flatten-tree
      fold/link-by-keywords))

(defn- save-fold! [path nodes]
  (let [stamp (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss") (java.util.Date.))
        f     (io/file "data/folds" (str (.getName (io/file path)) "." stamp ".edn"))]
    (io/make-parents f)
    (with-open [w (io/writer f)] (pp/pprint nodes w))
    (str f)))

(defn remove-source!
  "Retract the source stored for `path` and all its nodes (if any)."
  [conn path]
  (let [db  (d/db conn)
        src (d/q '[:find ?s . :in $ ?p :where [?s :source/path ?p]] db path)]
    (when src
      (let [nodes (d/q '[:find [?n ...] :in $ ?s :where [?n :node/source ?s]] db src)]
        (d/transact! conn (into [[:db/retractEntity src]]
                                (map (fn [n] [:db/retractEntity n]))
                                nodes))
        (count nodes)))))

(defn ingest-pdf!
  "Extract, fold with Claude and store `path`, replacing an earlier ingest of
  the same path. Prints progress.
  opts: :models  tier -> model id overrides (config.edn [:anthropic :models])
  Returns {:source-id .. :nodes n :fold-file ..}."
  ([conn client path] (ingest-pdf! conn client path {}))
  ([conn client path {:keys [models]}]
  (let [tree  (outline-pdf path)
        total (internal-count tree)
        done  (atom 0)
        summ  (fold/llm-summarizer client {:outline (fold/outline-text tree)
                                           :models  models
                                           :on-call #(println (format "folded %d/%d" (swap! done inc) total))})
        nodes (fold-tree summ tree)
        file  (save-fold! path nodes)
        tx    (fold/->tx {:title (:title tree) :path (str path)} nodes)]
    (remove-source! conn (str path))
    (d/transact! conn tx)
    {:source-id (:source/id (first tx)) :nodes (count nodes) :fold-file file})))
