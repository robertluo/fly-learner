;; bb ingest: see ingest! below
(ns robertluo.fly-learner.cli
  "Command-line tasks. App layer: the system's db, ingest and concepts.

    bb ingest data/sources/<file>.pdf|.epub|<folder of Markdown> ... [--skip <part>,..] [--outline]

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

(defn- options
  "Command-line `args` as {:paths [..] :skip [..] :outline? bool}."
  [args]
  (loop [[a & more] args, out {:paths []}]
    (cond (nil? a)              out
          (= "--outline" a)     (recur more (assoc out :outline? true))
          (= "--skip" a)        (if (seq more)
                                  (recur (rest more) (update out :skip (fnil into [])
                                                             (remove str/blank? (map str/trim (str/split (first more) #",")))))
                                  (throw (ex-info "--skip what? e.g. --skip Reference" {})))
          (str/starts-with? a "--") (throw (ex-info (str "Unknown option " a " (known: --skip <part>,.. --outline)") {}))
          :else                 (recur more (update out :paths conj a)))))

^:rct/test
(comment
  (options ["docs" "--skip" "Reference, Old" "--outline"]) ;=> {:paths ["docs"] :skip ["Reference" "Old"] :outline? true}
  (options ["a.pdf" "b.epub"]) ;=> {:paths ["a.pdf" "b.epub"]}
  )

(defn- print-preview!
  "Print `ingest/preview` of the source at `path`."
  [path {:keys [title type pages chars sections about estimate]}]
  (let [{:keys [manifest chapters left-out unlisted missing repo]} about]
    (println (format "%s: \"%s\", %d pages, %,d characters" path title pages chars))
    (when (= :markdown type)
      (println (format "  %d chapters, in the order of %s%s" chapters (or manifest "the folder (no manifest)")
                       (if-let [{:keys [web commit]} repo]
                         (str "; links out of the folder open " (or web (:root repo)) (when commit (str " at " (subs commit 0 (min 9 (count commit))))))
                         "")))
      (when (seq left-out) (println "  left out:" (str/join ", " left-out)))
      (when (seq unlisted) (println (format "  not in %s, so not in the book: %s" (or manifest "it") (str/join ", " unlisted))))
      (when (seq missing) (println "  listed but missing:" (str/join ", " missing))))
    (doseq [{:keys [title depth page chars]} sections]
      (println (format "  %-56s p.%-4s %,8d" (str (apply str (repeat (dec depth) "  ")) title) page chars)))
    (println (format "  Folding: %s calls, about $%.2f (a rough estimate; the steps after it, concepts to explorations, cost more)"
                     (str/join " + " (for [[tier n] (sort-by key (:calls estimate))] (str n " " (name tier))))
                     (:usd estimate)))))

(defn ingest!
  "Ingest the sources (PDF or EPUB files, folders of Markdown) at `args`: extract, fold with Claude and store each
  (replacing an earlier ingest of the same document, wherever the file is now), then resolve the keywords
  no concept covers yet into concepts and cluster them into topics again,
  judge what new concepts need first (concept.prereq), then find the key sentences of new passages and judge which could become
  explorations. Uses the running system's db when
  there is one, else opens it and closes it after. Prints progress and the
  cost. Returns {:sources [..] :concepts totals}.
  Options among the paths:
    --skip <part>,..  parts (manifest titles) or files of a Markdown folder
                      left out; kept: ingesting the folder again leaves them out
    --outline         only print what each source would make (chapters,
                      pages, a rough cost of folding it); no model, no db"
  [& args]
  (let [{:keys [paths skip outline?]} (options args)
        paths   (map source-path paths)
        missing (remove #(.exists (io/file %)) paths)
        unknown (remove ingest/source-type paths)]
    (when (empty? paths)
      (throw (ex-info "Which source? bb ingest data/sources/<file>.pdf (or .epub, or a folder of Markdown)" {})))
    (when (seq missing)
      (throw (ex-info (str "No such file or folder: " (str/join ", " missing)) {:missing (vec missing)})))
    (when (seq unknown)
      (throw (ex-info (str "Not a PDF, EPUB or Markdown: " (str/join ", " unknown)) {:unknown (vec unknown)})))
    (if outline?
      (let [models (:models (llm/settings (sys/load-config)))]
        (vec (for [p paths]
               (let [r (ingest/preview p {:skip skip :models models})]
                 (print-preview! p r)
                 r))))
    (let [cfg    (sys/load-config)
          models (:models (llm/settings cfg))
          client (llm/client (llm/settings cfg))
          own    (when-not (sys/conn) (sys/connect cfg))
          conn   (or (sys/conn) own)
          before (llm/cost)]
      (try
        (let [sources (vec (for [p paths]
                             (do (println "Ingesting" p)
                                 (let [r (ingest/ingest-source! conn client p (cond-> {:models models} skip (assoc :skip skip)))]
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
          (println "Transcribing new exercises: tables and formulas as the book shows them")
          (let [{:keys [transcribed]} (exercises/transcribe! conn client {:models models})]
            (println (format "  %d exercises transcribed" transcribed)))
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
          (when own (db/close own))))))))

(defn -main [& paths]
  (try
    (apply ingest! paths)
    (shutdown-agents)
    (System/exit 0)
    (catch Exception e
      (binding [*out* *err*] (println (ex-message e)))
      (System/exit 1))))
