;; bb ingest: see ingest! below
(ns robertluo.fly-learner.cli
  "Command-line tasks. App layer: the system's db, ingest and concepts.

    bb ingest data/sources/<file>.pdf ...

  runs `ingest!` inside the dev REPL when one is up (the db is the running
  system's), else as `clojure -M:ingest` (see bb.edn)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.concept.cluster :as cluster]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.server.study :as study]
            [robertluo.fly-learner.server.system :as sys]))

(defn source-path
  "`path` as stored for a source: relative to the working directory when
  inside it (the server opens it from there), so that ingesting the same file
  again, however it is spelled, replaces the earlier ingest."
  [path]
  (let [cwd (.toAbsolutePath (java.nio.file.Path/of "" (make-array String 0)))
        p   (.normalize (.toAbsolutePath (java.nio.file.Path/of (str path) (make-array String 0))))]
    (str (if (.startsWith p cwd) (.relativize cwd p) p))))

^:rct/test
(comment
  (source-path "./data/sources/../sources/x.pdf") ;=> "data/sources/x.pdf"
  (source-path "data/sources/x.pdf") ;=> "data/sources/x.pdf"
  (source-path "/elsewhere/x.pdf") ;=> "/elsewhere/x.pdf"
  )

(defn- spent
  "USD spent since the cost snapshot `before` (llm/cost)."
  [before]
  (- (:total (llm/cost) 0) (:total before 0)))

(defn ingest!
  "Ingest the PDFs at `paths`: extract, fold with Claude and store each
  (replacing an earlier ingest of the same file), then resolve the keywords
  no concept covers yet into concepts and cluster them into topics again. Uses the running system's db when
  there is one, else opens it and closes it after. Prints progress and the
  cost. Returns {:sources [..] :concepts totals}."
  [& paths]
  (let [paths   (map source-path paths)
        missing (remove #(.isFile (io/file %)) paths)]
    (when (empty? paths)
      (throw (ex-info "Which PDF? bb ingest data/sources/<file>.pdf" {})))
    (when (seq missing)
      (throw (ex-info (str "No such file: " (str/join ", " missing)) {:missing (vec missing)})))
    (let [cfg    (sys/load-config)
          models (get-in cfg [:anthropic :models])
          client (llm/client (:anthropic cfg))
          own    (when-not (sys/conn) (sys/connect cfg))
          conn   (or (sys/conn) own)
          before (llm/cost)]
      (try
        (let [sources (vec (for [p paths]
                             (do (println "Ingesting" p)
                                 (let [r (ingest/ingest-pdf! conn client p {:models models})]
                                   (println (format "  %d nodes; fold saved to %s" (:nodes r) (:fold-file r)))
                                   r))))
              _        (println "Resolving new keywords into concepts")
              concepts (concept/resolve! conn (concept/llm-resolver client {:models models})
                                         {:on-batch (fn [{:keys [new attached ignored]}]
                                                      (println (format "  batch: %d new concepts, %d keywords to existing ones, %d ignored"
                                                                       (or new 0) (or attached 0) (or ignored 0))))})]
          (println "Clustering concepts into topics")
          (let [{:keys [clusters kept named dropped successors]}
                (cluster/refresh! conn (cluster/llm-namer client {:models models}))
                moved (study/clusters-replaced! conn successors)]
            (println (format "  %d clusters: %d kept, %d new (named), %d gone" clusters kept named dropped))
            (when (seq moved)
              (println (format "  %d topic(s) you were learning regrouped: now learning their successors" (count moved)))))
          (println (format "Done: %d source(s); %d new concepts, %d keywords joined existing ones, %d ignored%s. Cost: $%.2f"
                           (count sources) (:new concepts) (:attached concepts) (:ignored concepts)
                           (if (seq (:missing concepts))
                             (str ", " (count (:missing concepts)) " left unresolved (run again to retry)")
                             "")
                           (spent before)))
          (println (if own
                     "Start the app to see it."
                     "Reload the app in the browser to see it."))
          {:sources sources :concepts concepts})
        (finally
          (when own (db/close own)))))))

(defn -main [& paths]
  (try
    (apply ingest! paths)
    (shutdown-agents)
    (System/exit 0)
    (catch Exception e
      (binding [*out* *err*] (println (ex-message e)))
      (System/exit 1))))
