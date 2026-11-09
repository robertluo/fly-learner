(ns robertluo.fly-learner.server.files
  "A source's files over HTTP, for the reader to show it:

    GET /files/sources/<id>              the original file (a PDF: pdf.js draws it)
    GET /files/sources/<id>/pages        an EPUB's pages as HTML, a JSON array
                                         (robertluo.fly-learner.ingest.epub/page-html)
    GET /files/sources/<id>/res/<entry>  an image (an entry of the EPUB) they show

  Only paths stored by ingest are ever read."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ingest.epub :as epub]
            [ring.util.response :as resp]))

(def ^:private media-types {:pdf "application/pdf" :epub "application/epub+zip"})

(defn- prefix [id] (str "/files/sources/" id))

(defonce ^:private pages-cache (atom {})) ; path -> [[modified length] json]

(defn- res-url
  "Where entry `entry` of source `id` (an EPUB) is served."
  [id entry]
  (str (prefix id) "/res/" (str/join "/" (map #(java.net.URLEncoder/encode ^String % "UTF-8") (str/split entry #"/")))))

(defn- pages-json
  "The EPUB at `path`'s pages, as JSON; computed once for a version of the file."
  [id path]
  (let [f       (io/file path)
        version [(.lastModified f) (.length f)]
        [v s]   (get @pages-cache path)]
    (if (= v version)
      s
      (let [s (json/write-value-as-string (epub/page-html path #(res-url id %)))]
        (swap! pages-cache assoc path [version s])
        s))))

(defn- cached [r] (resp/header r "Cache-Control" "private, max-age=3600"))

(defn response
  "The ring response for GET `uri` when it is a source's file, else nil."
  [conn uri]
  (when-let [[_ id more] (re-matches #"/files/sources/([0-9a-f-]{36})(/.*)?" uri)]
    (let [{:source/keys [path type]} (some->> (parse-uuid id) (db/source (d/db conn)))]
      (when (and path (.isFile (io/file path)))
        (cond
          (nil? more)
          (-> (resp/file-response path) (resp/content-type (media-types type "application/octet-stream")) cached)

          (and (= :epub type) (= "/pages" more))
          (-> (resp/response (pages-json id path)) (resp/content-type "application/json; charset=utf-8") cached)

          (and (= :epub type) (str/starts-with? more "/res/"))
          (when-let [{:keys [bytes type]} (epub/resource path (java.net.URLDecoder/decode (subs more 5) "UTF-8"))]
            (-> (resp/response (java.io.ByteArrayInputStream. bytes)) (resp/content-type type) cached)))))))
