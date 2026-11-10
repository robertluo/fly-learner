(ns robertluo.fly-learner.ingest.epub
  "EPUB source: the book's documents (XHTML, in spine order) and its table
  of contents (nav, else NCX), read as pages by
  robertluo.fly-learner.ingest.book.

  An EPUB has no pages of its own: its text is cut into pages of about
  book/page-chars characters, a document (chapter file) starting a new one,
  so that it is read and pointed at as a PDF is: by page."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [robertluo.fly-learner.ingest.book :as book])
  (:import (java.util.zip ZipFile)
           (org.jsoup Jsoup)
           (org.jsoup.nodes Element)
           (org.jsoup.parser Parser)))

;;=============================================================================
;; The package: metadata, spine, table of contents
;;=============================================================================

(defn- entry-text [^ZipFile zip name]
  (when-let [e (.getEntry zip name)]
    (with-open [in (.getInputStream zip e)]
      (slurp in :encoding "UTF-8"))))

(defn- xml [s] (Jsoup/parse ^String s "" (Parser/xmlParser)))

(defn- toc-depths
  "{[entry fragment] depth} from the nav document's toc (an <ol> per level)
  or an NCX's navPoints."
  [^ZipFile zip nav-path ncx-path]
  (if-let [nav (some->> nav-path (entry-text zip) xml)]
    (let [toc (or (first (filter #(re-find #"toc" (.attr ^Element % "epub:type")) (.select nav "nav")))
                  (first (.select nav "nav")))]
      (into {} (for [^Element a (some-> toc (.select "a[href]"))]
                 [(book/resolve-path nav-path (.attr a "href"))
                  (count (filter #(= "ol" (.tagName ^Element %)) (.parents a)))])))
    (when-let [ncx (some->> ncx-path (entry-text zip) xml)]
      (into {} (for [^Element c (.select ncx "navPoint > content[src]")]
                 [(book/resolve-path ncx-path (.attr c "src"))
                  (count (filter #(= "navPoint" (.tagName ^Element %)) (.parents c)))])))))

(defn- package
  "{:title :docs [entry-path ..] (spine order, linear ones) :toc {[entry frag] depth}}."
  [^ZipFile zip]
  (let [opf-path (some-> (entry-text zip "META-INF/container.xml") xml
                         (.selectFirst "rootfile") (.attr "full-path"))
        opf      (xml (or (entry-text zip opf-path) (throw (ex-info "Not an EPUB: no package document" {}))))
        items    (into {} (for [^Element i (.select opf "manifest > item")]
                            [(.attr i "id") {:path (first (book/resolve-path opf-path (.attr i "href")))
                                             :type (.attr i "media-type")
                                             :props (set (str/split (.attr i "properties") #"\s+"))}]))
        spine    (.selectFirst opf "spine")
        docs     (for [^Element r (.select opf "spine > itemref")
                       :when (not= "no" (.attr r "linear"))
                       :let [{:keys [path type]} (items (.attr r "idref"))]
                       :when (and path (re-find #"html" type))]
                   path)]
    {:title (some-> (.selectFirst opf "metadata > dc|title") .text str/trim not-empty)
     :docs  (vec docs)
     :toc   (toc-depths zip
                        (some #(when ((:props %) "nav") (:path %)) (vals items))
                        (some-> spine (.attr "toc") not-empty items :path))}))

;;=============================================================================
;; The book
;;=============================================================================

(defn- read-package
  "The EPUB at `path` as a book's package (robertluo.fly-learner.ingest.book):
  its documents parsed, names the zip's entry paths."
  [path]
  (with-open [zip (ZipFile. (io/file path))]
    (let [{:keys [title docs toc]} (package zip)]
      {:title (or title (.getName (io/file path)))
       :toc   toc
       :docs  (vec (for [d docs :let [s (entry-text zip d)] :when s]
                     [d (Jsoup/parse ^String s "" (Parser/xmlParser))]))})))

(defn extract
  "Return {:title .. :pages n :blocks [...]} (as robertluo.fly-learner.ingest.pdf/extract)."
  [path]
  (book/extract (read-package path)))

(defn page-html
  "The book at `path` as pages of HTML to show: [html ..], page p at p - 1.
  `resource-url`: an entry path (an image) -> the URL it is served at."
  [path resource-url]
  (book/page-html (read-package path) resource-url))

(defn resource
  "The bytes and media type of entry `entry` of the book at `path`, nil when
  there is none: {:bytes :type}."
  [path entry]
  (with-open [zip (ZipFile. (io/file path))]
    (when-let [e (.getEntry zip ^String entry)]
      (when-not (.isDirectory e)
        {:bytes (with-open [in (.getInputStream zip e)] (.readAllBytes in))
         :type  (or (book/media-type entry) "application/octet-stream")}))))
