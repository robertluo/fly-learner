;; bb ingest: see ingest! below
(ns robertluo.fly-learner.cli
  "Command-line tasks. App layer: the system's db, ingest and concepts.

    bb ingest data/sources/<file>.pdf|.epub|<folder of Markdown> ... [--skip <part>,..] [--outline]

  runs `ingest!` inside the dev REPL when one is up (the db is the running
  system's), else as `clojure -M:ingest` (see bb.edn)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.server.pipeline :as pipeline]
            [robertluo.fly-learner.server.settings :as settings]
            [robertluo.fly-learner.server.system :as sys]))

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
        paths   (map pipeline/source-path paths)
        missing (remove #(.exists (io/file %)) paths)
        unknown (remove ingest/source-type paths)]
    (when (empty? paths)
      (throw (ex-info "Which source? bb ingest data/sources/<file>.pdf (or .epub, or a folder of Markdown)" {})))
    (when (seq missing)
      (throw (ex-info (str "No such file or folder: " (str/join ", " missing)) {:missing (vec missing)})))
    (when (seq unknown)
      (throw (ex-info (str "Not a PDF, EPUB or Markdown: " (str/join ", " unknown)) {:unknown (vec unknown)})))
    (if outline?
      (let [models (:models (settings/llm (sys/load-config)))]
        (vec (for [p paths]
               (let [r (ingest/preview p {:skip skip :models models})]
                 (print-preview! p r)
                 r))))
    (let [cfg    (sys/load-config)
          models (:models (settings/llm cfg))
          client (settings/client cfg)
          own    (when-not (sys/conn) (sys/connect cfg))
          conn   (or (sys/conn) own)]
      (try
        (let [r (pipeline/run! conn client paths (cond-> {:models models} skip (assoc :skip skip)))]
          (println (if own
                     "Start the app to see it."
                     "Reload the app in the browser to see it."))
          (dissoc r :cost))
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
