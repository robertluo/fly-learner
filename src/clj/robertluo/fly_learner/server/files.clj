(ns robertluo.fly-learner.server.files
  "A source's files over HTTP, for the reader to show it:

    GET /files/sources/<id>              the original file (a PDF: pdf.js draws it)
    GET /files/sources/<id>/pages        an EPUB's or a Markdown folder's pages as HTML,
                                         a JSON array (robertluo.fly-learner.ingest/page-html)
    GET /files/sources/<id>/res/<name>   an image they show (an entry of the EPUB, a file
                                         of the folder's repository)

  Only paths stored by ingest are ever read. An image is sent sandboxed: one
  opened by itself (an SVG of a repository) runs no script."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ingest :as ingest]
            [ring.util.response :as resp]))

(def ^:private media-types {:pdf "application/pdf" :epub "application/epub+zip"})

(defn- prefix [id] (str "/files/sources/" id))

(defonce ^:private pages-cache (atom {})) ; source id -> [version json]

(defn- res-url
  "Where `name` (an image) of source `id` is served."
  [id name]
  (str (prefix id) "/res/" (str/join "/" (map #(java.net.URLEncoder/encode ^String % "UTF-8") (str/split name #"/")))))

(defn- pages-json
  "The pages of `source` as JSON; computed once for a version of them."
  [{:source/keys [id] :as source}]
  (let [version (ingest/page-version source)
        [v s]   (get @pages-cache id)]
    (if (= v version)
      s
      (let [s (json/write-value-as-string (ingest/page-html source #(res-url id %)))]
        (swap! pages-cache assoc id [version s])
        s))))

(defn- cached [r] (resp/header r "Cache-Control" "private, max-age=3600"))

(defn response
  "The ring response for GET `uri` when it is a source's file, else nil."
  [conn uri]
  (when-let [[_ id more] (re-matches #"/files/sources/([0-9a-f-]{36})(/.*)?" uri)]
    (let [{:source/keys [path type] :as source} (some->> (parse-uuid id) (db/source (d/db conn)))]
      (when (and path (.exists (io/file path)))
        (cond
          (nil? more)
          (when (.isFile (io/file path))
            (-> (resp/file-response path) (resp/content-type (media-types type "application/octet-stream")) cached))

          (and (ingest/html-pages? type) (= "/pages" more))
          (-> (resp/response (pages-json source)) (resp/content-type "application/json; charset=utf-8") cached)

          (and (ingest/html-pages? type) (str/starts-with? more "/res/"))
          (when-let [{:keys [bytes type]} (ingest/resource source (java.net.URLDecoder/decode (subs more 5) "UTF-8"))]
            (-> (resp/response (java.io.ByteArrayInputStream. bytes))
                (resp/content-type type)
                (resp/header "Content-Security-Policy" "sandbox")
                cached)))))))
