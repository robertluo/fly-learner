(ns robertluo.fly-learner.server.pipeline
  "Ingesting sources and everything that follows, as `bb ingest` and the
  settings page run it (app layer): fold each source, resolve its keywords
  into concepts, cluster topics again (goals on a regrouped topic move to
  its successor), judge what new concepts need first, find key sentences,
  the books' exercises (and transcribe them), and the passages worth
  exploring. Prints progress and the cost as it goes."
  (:refer-clojure :exclude [run!])
  (:require [robertluo.fly-learner.concept :as concept]
            [robertluo.fly-learner.concept.cluster :as cluster]
            [robertluo.fly-learner.concept.prereq :as prereq]
            [robertluo.fly-learner.exercises :as exercises]
            [robertluo.fly-learner.explore.knowledge :as explore]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.keypoints :as keypoints]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.server.study :as study]))

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

(defn run!
  "Ingest the sources at `paths` (as stored, `source-path`) into `conn` with
  model `client`, then everything that follows (see the ns doc). opts:
  :models (tier -> model), :skip (a Markdown folder's parts left out).
  Returns {:sources [{:source-id :nodes ..}] :concepts totals :cost usd}."
  [conn client paths {:keys [models skip]}]
  (let [before   (llm/cost)
        opts     {:models models}
        sources  (vec (for [p paths]
                        (do (println "Ingesting" p)
                            (let [r (ingest/ingest-source! conn client p (cond-> opts skip (assoc :skip skip)))]
                              (println (format "  %d nodes; fold saved to %s" (:nodes r) (:fold-file r)))
                              r))))
        _        (println "Resolving new keywords into concepts")
        concepts (concept/resolve! conn (concept/llm-resolver client opts)
                                   {:on-batch (fn [{:keys [new attached ignored]}]
                                                (println (format "  batch: %d new concepts, %d keywords to existing ones, %d ignored"
                                                                 (or new 0) (or attached 0) (or ignored 0))))})]
    (println "Clustering concepts into topics")
    (let [{:keys [clusters kept named dropped successors]} (cluster/refresh! conn (cluster/llm-namer client opts))
          moved (study/clusters-replaced! conn successors)]
      (println (format "  %d clusters: %d kept, %d new (named), %d gone" clusters kept named dropped))
      (when (seq moved)
        (println (format "  %d topic(s) you were learning regrouped: now learning their successors" (count moved)))))
    (println "Judging what new concepts need first")
    (let [{:keys [judged claims hidden]} (prereq/judge! conn client nil opts)]
      (println (format "  %d concepts judged: %d prerequisites, %d hidden concepts (background the sources assume)"
                       judged claims hidden)))
    (println "Finding the key sentences of new passages")
    (let [{:keys [passages keypoints]} (keypoints/extract! conn client opts)]
      (println (format "  %d passages, %d key sentences" passages keypoints)))
    (println "Finding the exercises of new sources")
    (let [{:keys [exercises solutions]} (exercises/extract! conn client opts)]
      (println (format "  %d exercises, %d with the author's solution" exercises solutions)))
    (println "Transcribing new exercises: tables and formulas as the book shows them")
    (let [{:keys [transcribed]} (exercises/transcribe! conn client opts)]
      (println (format "  %d exercises transcribed" transcribed)))
    (println "Finding passages to explore")
    (let [{:keys [judged proposed]} (explore/propose! conn client opts)]
      (println (format "  %d passages judged, %d can be explored" judged proposed)))
    (let [cost (spent before)]
      (println (format "Done: %d source(s); %d new concepts, %d keywords joined existing ones, %d ignored%s. Cost: $%.2f"
                       (count sources) (:new concepts) (:attached concepts) (:ignored concepts)
                       (if (seq (:missing concepts))
                         (str ", " (count (:missing concepts)) " left unresolved (run again to retry)")
                         "")
                       cost))
      {:sources sources :concepts concepts :cost cost})))
