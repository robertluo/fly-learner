(ns robertluo.fly-learner.ingest
  "Source -> knowledge DAG -> database.

  A source is its document, not where the file is: `same-source` finds the
  one stored for a file by its content (`file-hash`), wherever it was, else
  by its path (the file changed in place), else by its text (moved and
  changed: a source whose file is gone from its path, sharing most of the
  text, `moved-overlap`; a document whose old file is still in place is a
  new one, e.g. a second edition kept beside the first). Ingesting it again keeps its
  :source/id, so what refers to the source (ideas, bookmarks, exercises,
  explorations) keeps referring to it; the path is only where to read it.
  A new source's id derives from its content (`source-id`): the same
  document gets the same id in a database built again."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
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

(defn file-hash
  "SHA-256 of the bytes of the file at `path`, in hex."
  [path]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream (io/file path))]
      (let [buf (byte-array 65536)]
        (loop []
          (let [n (.read in buf)]
            (when (pos? n) (.update md buf 0 n) (recur))))))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(defn source-id
  "The id of a new source with content `hash`."
  [hash]
  (java.util.UUID/nameUUIDFromBytes (.getBytes (str "source#" hash) "UTF-8")))

(defn- words [s]
  (re-seq #"[\p{L}\p{N}]+" (str/lower-case s)))

(def ^:private shingle-size 5)

(defn shingles
  "The text of `texts` as a set of overlapping word runs (hashed): two
  versions of a document share most of them, however their paragraphs
  were edited or regrouped."
  [texts]
  (into #{} (comp (mapcat #(partition shingle-size 1 (words %))) (map hash)) texts))

(defn overlap
  "How much of the smaller of shingle sets `a` and `b` the other contains, in [0, 1]."
  [a b]
  (if (or (empty? a) (empty? b))
    0.0
    (/ (count (set/intersection a b)) (double (min (count a) (count b))))))

(def moved-overlap
  "Share of the text two versions of a document have in common at least."
  0.6)

(defn tree-texts
  "The paragraph texts of section tree `tree` (outline-pdf)."
  [tree]
  (keep :text (tree-seq :children :children tree)))

(defn- source-texts [db sid]
  (d/q '[:find [?t ...] :in $ ?sid :where [?s :source/id ?sid] [?n :node/source ?s] [?n :node/text ?t]] db sid))

(defn same-source
  "The source stored for the file at `path` with content `hash` and
  paragraph `texts`, as {:source/id :by ..}, nil when new:
    :by :content  the same bytes, wherever the file was
    :by :path     the file at this path changed
    :by :text     moved and changed: of the sources whose file is gone from
                  its path, the one sharing most text (:overlap, at least
                  `moved-overlap`), :was its old path"
  [db {:keys [path hash texts]}]
  (or (some->> (d/q '[:find ?id . :in $ ?h :where [?s :source/hash ?h] [?s :source/id ?id]] db hash)
               (hash-map :by :content :source/id))
      (some->> (d/q '[:find ?id . :in $ ?p :where [?s :source/path ?p] [?s :source/id ?id]] db path)
               (hash-map :by :path :source/id))
      (let [gone (for [[id p] (d/q '[:find ?id ?p :where [?s :source/id ?id] [?s :source/path ?p]] db)
                       :when (not (.isFile (io/file p)))]
                   [id p])]
        (when (seq gone)
          (let [mine (shingles texts)
                [id p o] (->> gone
                              (map (fn [[id p]] [id p (overlap mine (shingles (source-texts db id)))]))
                              (apply max-key #(nth % 2)))]
            (when (<= moved-overlap o)
              {:source/id id :by :text :overlap o :was p}))))))

(defn remove-source!
  "Retract source `id`, all its nodes and its exercises (if any)."
  [conn id]
  (let [db  (d/db conn)
        src (d/entid db [:source/id id])]
    (when src
      (let [nodes (d/q '[:find [?n ...] :in $ ?s :where [?n :node/source ?s]] db src)
            xs    (d/q '[:find [?x ...] :in $ ?s :where [?x :exercise/source ?s]] db src)]
        (d/transact! conn (into [[:db/retractEntity src]]
                                (map (fn [e] [:db/retractEntity e]))
                                (concat nodes xs)))
        (count nodes)))))

(defn ingest-pdf!
  "Extract, fold with Claude and store `path`, replacing an earlier ingest of
  the same source (`same-source`: the same content, the same path, or most
  of the text with the old file gone), whose id it keeps. Prints progress.
  opts: :models  tier -> model id overrides (config.edn [:anthropic :models])
  Returns {:source-id .. :nodes n :fold-file ..}."
  ([conn client path] (ingest-pdf! conn client path {}))
  ([conn client path {:keys [models]}]
  (let [hash  (file-hash path)
        tree  (outline-pdf path)
        found (same-source (d/db conn) {:path (str path) :hash hash :texts (tree-texts tree)})
        old   (:source/id found)
        _     (when (= :text (:by found))
                (println (format "  the document that was at %s, moved and changed (%.0f%% of its text the same)"
                                 (:was found) (* 100 (:overlap found)))))
        total (internal-count tree)
        done  (atom 0)
        summ  (fold/llm-summarizer client {:outline (fold/outline-text tree)
                                           :models  models
                                           :on-call #(println (format "folded %d/%d" (swap! done inc) total))})
        nodes (fold-tree summ tree)
        file  (save-fold! path nodes)
        tx    (fold/->tx {:id (or old (source-id hash)) :title (:title tree) :path (str path) :hash hash} nodes)]
    (when old (remove-source! conn old))
    (d/transact! conn tx)
    {:source-id (:source/id (first tx)) :nodes (count nodes) :fold-file file})))
