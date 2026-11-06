(ns ingest-task
  "bb ingest: run robertluo.fly-learner.cli/ingest! inside the dev REPL when one
  is up (the running app holds the db; only one process may write it), else
  in a new JVM (clojure -M:ingest)."
  (:require [babashka.fs :as fs]
            [babashka.tasks :refer [clojure]]
            [bencode.core :as b]
            [clojure.string :as str]))

(defn- repl-socket
  "A socket to the dev REPL (.nrepl-port), or nil when none is up."
  []
  (when-let [port (when (fs/exists? ".nrepl-port") (parse-long (str/trim (slurp ".nrepl-port"))))]
    (try (java.net.Socket. "localhost" (int port)) (catch Exception _ nil))))

(defn- text [bs] (when bs (String. ^bytes bs "UTF-8")))

(defn- eval-printing!
  "Evaluate `code` over nREPL on `sock`, printing its output as it comes.
  Returns true when it threw."
  [^java.net.Socket sock code]
  (let [out (.getOutputStream sock)
        in  (java.io.PushbackInputStream. (.getInputStream sock))]
    (b/write-bencode out {"op" "eval" "id" "ingest" "code" code})
    (loop [failed? false]
      (let [m (b/read-bencode in)]
        (when-let [s (text (get m "out"))] (print s) (flush))
        (when-let [s (text (get m "err"))] (binding [*out* *err*] (print s) (flush)))
        (let [failed? (or failed? (some? (get m "ex")))]
          (if (some #(= "done" (text %)) (get m "status"))
            failed?
            (recur failed?)))))))

(defn run [paths]
  (when (empty? paths)
    (println "Usage: bb ingest data/sources/<file>.pdf ...")
    (System/exit 1))
  (if-let [sock (repl-socket)]
    (with-open [s sock]
      (println "Using the running dev REPL")
      (when (eval-printing! s (str "(apply (requiring-resolve 'robertluo.fly-learner.cli/ingest!) "
                                   (pr-str (vec paths)) ")"))
        (System/exit 1)))
    (apply clojure "-M:ingest" paths)))
