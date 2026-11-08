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
            [robertluo.fly-learner.concept.prereq :as prereq]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.explore.knowledge :as explore]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.exercises :as exercises]
            [robertluo.fly-learner.keypoints :as keypoints]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.server.study :as study]
            [robertluo.fly-learner.server.system :as sys]))

(defn source-path
  "`path` as stored for a source: relative to the working directory when
  inside it (the server opens it from there). Only where the file is: a
  source is found again by its content (robertluo.fly-learner.ingest/same-source)."
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
  (replacing an earlier ingest of the same document, wherever the file is now), then resolve the keywords
  no concept covers yet into concepts and cluster them into topics again,
  judge what new concepts need first (concept.prereq), then find the key sentences of new passages and judge which could become
  explorations. Uses the running system's db when
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
          (println "Judging what new concepts need first")
          (let [{:keys [judged claims hidden]} (prereq/judge! conn client nil {:models models})]
            (println (format "  %d concepts judged: %d prerequisites, %d hidden concepts (background the sources assume)"
                             judged claims hidden)))
          (println "Finding the key sentences of new passages")
          (let [{:keys [passages keypoints]} (keypoints/extract! conn client {:models models})]
            (println (format "  %d passages, %d key sentences" passages keypoints)))
          (println "Finding the exercises of new sources")
          (let [{:keys [exercises solutions]} (exercises/extract! conn client {:models models})]
            (println (format "  %d exercises, %d with the author's solution" exercises solutions)))
          (println "Finding passages to explore")
          (let [{:keys [judged proposed]} (explore/propose! conn client {:models models})]
            (println (format "  %d passages judged, %d can be explored" judged proposed)))
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
