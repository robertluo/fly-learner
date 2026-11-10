(ns robertluo.fly-learner.server.ingests
  "Adding sources from the settings page (app layer).

  A file dropped there (a PDF, an EPUB, a Markdown file, or a zip of any of
  these or of a folder of Markdown) is kept under `upload-dir` (`upload!`;
  a zip unpacked beside it, safely: no entry outside its folder, bounded in
  size and count). What it holds that can be a source (`candidates`: each
  PDF and EPUB, and the folder holding all its Markdown) is read without a
  model (`describe`: title, pages, a rough cost, a folder's parts, whether
  it is already in the library). Ingesting is asked for separately
  (`start!`) and runs in the background, one job at a time
  (robertluo.fly-learner.server.pipeline), what it prints kept as the job's
  log (`jobs`)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.server.pipeline :as pipeline])
  (:import (java.io File)
           (java.util.concurrent ExecutorService Executors)
           (java.util.zip ZipInputStream)))

(def upload-dir "Where dropped files are kept." "data/sources/uploads")

(def max-upload "Bytes a dropped file may have." (* 512 1024 1024))

(def ^:private max-unzipped (* 4 1024 1024 1024))
(def ^:private max-entries 20000)

;;=============================================================================
;; Uploads
;;=============================================================================

(defn safe-name
  "File name `s` fit to keep: its last path part, other characters than
  letters, digits, space, dot, dash and underscore as _, no leading dot."
  [s]
  (let [n (-> (str s) (str/split #"[/\\]") last (or "")
              (str/replace #"[^\p{L}\p{N} ._\-]" "_")
              (str/replace #"^[.\s]+" "")
              str/trim)
        n (if (> (count n) 120) (subs n (- (count n) 120)) n)]
    (if (str/blank? n) "upload" n)))

^:rct/test
(comment
  (safe-name "../../etc/passwd") ;=> "passwd"
  (safe-name "C:\\docs\\My Book (2nd).pdf") ;=> "My Book _2nd_.pdf"
  (safe-name ".hidden.md") ;=> "hidden.md"
  )

(defn- fresh
  "A file named `n` in `dir` that does not exist yet (n, n-2, n-3, ..)."
  ^File [dir n]
  (let [[_ stem ext] (re-matches #"(.*?)((?:\.[A-Za-z0-9]+)?)" n)]
    (->> (cons n (map #(str stem "-" % ext) (iterate inc 2)))
         (map #(io/file dir %))
         (remove #(.exists ^File %))
         first)))

(defn- ignored-entry? [^String n]
  (or (str/includes? n "__MACOSX/")
      (re-find #"(^|/)\.[^/]*$|(^|/)\._" n)))

(defn unzip!
  "Unpack zip `f` into the new folder `dir`; entries that would land
  outside it are refused, the rest bounded (`max-unzipped` bytes,
  `max-entries`). Returns `dir`."
  [^File f ^File dir]
  (let [root (.getCanonicalFile dir)]
    (.mkdirs root)
    (with-open [zin (ZipInputStream. (io/input-stream f))]
      (loop [total 0, n 0]
        (when-let [e (.getNextEntry zin)]
          (let [out (.getCanonicalFile (io/file root (.getName e)))]
            (when-not (.startsWith (.toPath out) (.toPath root))
              (throw (ex-info (str "A zip entry outside its folder: " (.getName e)) {:entry (.getName e)})))
            (when (> n max-entries) (throw (ex-info "Too many files in the zip" {})))
            (if (or (.isDirectory e) (ignored-entry? (.getName e)))
              (recur total (inc n))
              (do (io/make-parents out)
                  (let [written (with-open [o (io/output-stream out)]
                                  (let [buf (byte-array 65536)]
                                    (loop [w 0]
                                      (let [k (.read zin buf)]
                                        (if (pos? k)
                                          (do (when (> (+ total w k) max-unzipped)
                                                (throw (ex-info "The zip unpacks to too much" {})))
                                              (.write o buf 0 k)
                                              (recur (+ w k)))
                                          w)))))]
                    (recur (+ total written) (inc n)))))))))
    root))

(defn- files-under [^File dir]
  (->> (file-seq dir)
       (filter #(.isFile ^File %))
       (remove #(ignored-entry? (str/replace (str (.relativize (.toPath dir) (.toPath ^File %))) File/separator "/")))))

(defn candidates
  "What file or folder `f` (a kept upload, or its unpacked zip) holds that
  can be a source: each PDF and EPUB, and the folder holding all its
  Markdown (their deepest common folder), as paths. A Markdown file alone
  is a source of its own."
  [^File f]
  (if-not (.isDirectory f)
    [(pipeline/source-path f)]
    (let [files (files-under f)
          md    (filter #(re-find #"(?i)\.(md|markdown)$" (.getName ^File %)) files)
          books (filter #(#{:pdf :epub} (ingest/source-type %)) files)
          root  (when (seq md)
                  (reduce (fn [^File a ^File b]
                            (loop [^File a a]
                              (if (.startsWith (.toPath b) (.toPath a)) a (recur (.getParentFile a)))))
                          (map #(.getParentFile ^File %) md)))]
      (mapv pipeline/source-path (concat (when root [root]) (sort books))))))

(defn- known-title
  "The title of the source already in the library with this content, if any."
  [db path type]
  (let [h (ingest/content-hash path type {})]
    (d/q '[:find ?t . :in $ ?h :where [?s :source/hash ?h] [?s :source/title ?t]] db h)))

(defn describe
  "What ingesting `path` would make, without a model: {:path :type :title
  :pages :chars :usd (a rough cost of folding) :parts [{:title :skip}] (a
  Markdown folder's top-level parts, :skip what leaves one out) :known
  (the title it already has in the library)}, or {:path :error}."
  [db models path]
  (try
    (let [type (ingest/source-type path)
          {:keys [title pages chars about estimate]} (ingest/preview path {:models models})]
      (cond-> {:path path :type type :title title :pages pages :chars chars :usd (:usd estimate)}
        (seq (:parts about)) (assoc :parts (:parts about) :chapters (:chapters about))
        true                 (assoc :known (known-title db path type))))
    (catch Exception e
      {:path path :error (or (ex-message e) (str (class e)))})))

(defn upload!
  "Keep the file named `name` with content `in` (an InputStream) under
  `upload-dir` (unpacked, if a zip) and describe what it holds:
  {:file name :candidates [(describe ..)]}. Throws on a file of no kind it
  reads, or too big."
  [db models name in]
  (let [n    (safe-name name)
        kind (some-> (re-find #"(?i)\.([a-z0-9]+)$" n) second str/lower-case)]
    (when-not (#{"pdf" "epub" "md" "markdown" "zip"} kind)
      (throw (ex-info (str "Not a file I read (PDF, EPUB, Markdown or a zip of them): " n) {:status 400})))
    (let [dir (io/file upload-dir)
          _   (.mkdirs dir)
          f   (fresh dir n)]
      (with-open [o (io/output-stream f)]
        (let [buf (byte-array 65536)]
          (loop [w 0]
            (let [k (.read ^java.io.InputStream in buf)]
              (when (pos? k)
                (when (> (+ w k) max-upload)
                  (.close o) (.delete f)
                  (throw (ex-info "The file is too big" {:status 413})))
                (.write o buf 0 k)
                (recur (+ w k)))))))
      (let [target (if (= "zip" kind)
                     (let [out (fresh dir (str/replace (.getName f) #"(?i)\.zip$" ""))]
                       (try (unzip! f out)
                            (catch Exception e
                              ;; nothing of a refused zip stays
                              (doseq [^File x (reverse (file-seq out))] (.delete x))
                              (throw e))
                            (finally (.delete f))))
                     f)]
        {:file n :candidates (mapv #(describe db models %) (candidates target))}))))

(defn upload-response
  "The ring response to POST /files/upload?name=<file name> (the file as the
  body): what it holds, as JSON (`upload!`)."
  [conn models {:keys [query-string body]}]
  (let [name (some-> (re-find #"(?:^|&)name=([^&]*)" (str query-string)) second
                     (java.net.URLDecoder/decode "UTF-8"))
        json #(-> {:status %1 :headers {"Content-Type" "application/json; charset=utf-8"}
                   :body (json/write-value-as-string %2)})]
    (try
      (if (and name body)
        (json 200 (upload! (d/db conn) models name body))
        (json 400 {:error "Which file?"}))
      (catch clojure.lang.ExceptionInfo e
        (json (or (:status (ex-data e)) 400) {:error (ex-message e)})))))

;;=============================================================================
;; Jobs
;;=============================================================================

(defonce ^:private jobs* (atom {}))
(defonce ^:private ^ExecutorService runner (Executors/newSingleThreadExecutor))

(def ^:private log-lines 300)

(defn- log!
  "Add `line` to job `id`'s log; a fold's progress replaces the one before."
  [id line]
  (swap! jobs* update-in [id :ingest/log]
         (fn [log]
           (let [progress? #(re-find #"^folded \d+/\d+$" (str %))
                 log       (if (and (progress? line) (progress? (peek log))) (pop log) log)
                 log       (conj (or log []) line)]
             (if (> (count log) log-lines) (subvec log (- (count log) log-lines)) log)))))

(defn- line-writer
  "A Writer handing each line written to (on-line line)."
  ^java.io.Writer [on-line]
  (let [sb  (StringBuilder.)
        put (fn [^String s]
              (locking sb
                (.append sb s)
                (loop []
                  (let [i (.indexOf sb "\n")]
                    (when (>= i 0)
                      (on-line (.substring sb 0 i))
                      (.delete sb 0 (inc i))
                      (recur))))))]
    (proxy [java.io.Writer] []
      (write
        ([x] (put (cond (string? x) x (integer? x) (str (char x)) :else (String. ^chars x))))
        ([x off len] (put (if (string? x)
                            (subs x off (+ off len))
                            (String. ^chars x (int off) (int len))))))
      (flush [])
      (close []))))

(defn jobs
  "The ingest jobs of this run of the app, newest first."
  []
  (->> (vals @jobs*) (sort-by :ingest/at #(compare %2 %1)) vec))

(defn job [id] (get @jobs* id))

(defn under-sources?
  "Is `path` a file or folder under data/sources (what the page may ingest)?"
  [path]
  (let [root (.getCanonicalFile (io/file "data/sources"))
        f    (.getCanonicalFile (io/file (str path)))]
    (and (.exists f) (.startsWith (.toPath f) (.toPath root)))))

(defn start!
  "Ingest `paths` (with `skip`, a Markdown folder's parts left out) in the
  background, after the jobs before it. `client` is a delay of the model
  client. Returns the job."
  [conn client models {:keys [paths skip]}]
  (let [id  (random-uuid)
        job {:ingest/id id :ingest/paths (vec paths) :ingest/skip (vec skip)
             :ingest/status :queued :ingest/at (java.util.Date.) :ingest/log []}]
    (swap! jobs* assoc id job)
    (.submit runner
             ^Runnable
             (fn []
               (swap! jobs* assoc-in [id :ingest/status] :running)
               (try
                 (let [r (binding [*out* (line-writer #(log! id %))]
                           (pipeline/run! conn @client paths (cond-> {:models models} (seq skip) (assoc :skip skip))))]
                   (swap! jobs* update id assoc :ingest/status :done :ingest/finished (java.util.Date.)
                          :ingest/sources (mapv :source-id (:sources r)) :ingest/cost (:cost r)))
                 (catch Throwable e
                   (log! id (str "Failed: " (or (ex-message e) (str (class e)))))
                   (swap! jobs* update id assoc :ingest/status :failed :ingest/finished (java.util.Date.)
                          :ingest/error (or (ex-message e) (str (class e))))))))
    job))

(defn forget!
  "Take finished job `id` off the list; a queued or running one stays."
  [id]
  (when (#{:done :failed} (:ingest/status (job id)))
    (swap! jobs* dissoc id)
    true))
