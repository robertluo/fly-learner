(ns robertluo.fly-learner.ingest.epub-test
  "An EPUB read as pages: blocks for the knowledge base, HTML to show,
  cut at the same places."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.ingest.epub :as epub])
  (:import (java.util.zip ZipEntry ZipOutputStream)))

(defn- xhtml [body]
  (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE html>\n"
       "<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:epub=\"http://www.idpf.org/2007/ops\">"
       "<head><title>t</title><script>alert(1)</script></head><body>" body "</body></html>"))

(def ^:private files
  {"mimetype" "application/epub+zip"
   "META-INF/container.xml"
   "<?xml version=\"1.0\"?><container><rootfiles><rootfile full-path=\"OEBPS/content.opf\"/></rootfiles></container>"
   "OEBPS/content.opf"
   (str "<?xml version=\"1.0\"?><package xmlns=\"http://www.idpf.org/2007/opf\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">"
        "<metadata><dc:title>A Small Book</dc:title></metadata><manifest>"
        "<item id=\"nav\" href=\"nav.xhtml\" properties=\"nav\" media-type=\"application/xhtml+xml\"/>"
        "<item id=\"cover\" href=\"text/cover.xhtml\" media-type=\"application/xhtml+xml\"/>"
        "<item id=\"c1\" href=\"text/c1.xhtml\" media-type=\"application/xhtml+xml\"/>"
        "<item id=\"c2\" href=\"text/c2.xhtml\" media-type=\"application/xhtml+xml\"/>"
        "<item id=\"fig\" href=\"img/f1.svg\" media-type=\"image/svg+xml\"/>"
        "</manifest><spine><itemref idref=\"cover\" linear=\"no\"/><itemref idref=\"c1\"/><itemref idref=\"c2\"/></spine></package>")
   "OEBPS/nav.xhtml"
   (xhtml (str "<nav epub:type=\"toc\"><ol>"
               "<li><a href=\"text/c1.xhtml\">1 Procedures</a><ol><li><a href=\"text/c1.xhtml#s11\">1.1 Elements</a></li></ol></li>"
               "<li><a href=\"text/c2.xhtml#c2\">2 Data</a></li></ol></nav>"))
   "OEBPS/text/cover.xhtml" (xhtml "<h1>Cover</h1>")
   "OEBPS/text/c1.xhtml"
   (xhtml (str "<section><nav class=\"header\"><p>Next: <a href=\"c2.xhtml\">2</a></p></nav>"
               "<h2 class=\"chapter\"><span>1</span><span>Procedures</span></h2>"
               "<p>A chapter opens.</p>"
               "<a id=\"s11\"></a><h3><span>1.1</span><span>Elements</span></h3>"
               "<p onclick=\"evil()\">If you present Lisp with a number<a href=\"#FOOT1\"><sup>1</sup></a></p>"
               "<div class=\"lisp\"><pre>(+ 1 2)\n3</pre></div>"
               "<h5>Unlisted heading</h5>"
               "<p>Exercise 1.1: Evaluate <a href=\"c2.xhtml#c2\">the next chapter</a>.</p>"
               "<figure><object data=\"../img/f1.svg\" type=\"image/svg+xml\">SVG</object>"
               "<figcaption><p>Figure 1.1: A tree.</p></figcaption></figure>"
               "<div class=\"footnote\"><h4>Footnotes</h4><div id=\"FOOT1\"><p><sup>1</sup> A footnote about numbers.</p></div></div>"
               "</section>"))
   "OEBPS/text/c2.xhtml"
   (xhtml (str "<a id=\"c2\"></a><h2>2 Data</h2>"
               (str/join (for [i (range 8)] (str "<p>Paragraph " i " " (str/join " " (repeat 90 "word")) ".</p>")))
               "<p><a href=\"javascript:evil()\">bad</a> and <a href=\"https://example.org\">out</a>.</p>"))
   "OEBPS/img/f1.svg" "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 10 10\"/>"})

(defn- book []
  (let [f (io/file (System/getProperty "java.io.tmpdir") (str "fly-learner-" (random-uuid) ".epub"))]
    (with-open [z (ZipOutputStream. (io/output-stream f))]
      (doseq [[name text] files]
        (.putNextEntry z (ZipEntry. ^String name))
        (.write z (.getBytes ^String text "UTF-8"))
        (.closeEntry z)))
    f))

(deftest an-epub-is-read-as-pages
  (let [f (book)]
    (try
      (let [{:keys [title pages blocks]} (epub/extract (str f))]
        (is (= "A Small Book" title))
        (is (= :epub (ingest/source-type (str f))))
        (testing "headings from the table of contents; one it does not list, deeper than the one before"
          (is (= [[1 "1 Procedures" 1] [2 "1.1 Elements" 1] [4 "Unlisted heading" 1] [1 "2 Data" 2]]
                 (map (juxt :depth :title :page) (filter #(= :heading (:kind %)) blocks)))))
        (testing "code joins its paragraph; a footnote follows the paragraph citing it"
          (is (= ["A chapter opens."
                  "If you present Lisp with a number1\n(+ 1 2)\n3"
                  "1 A footnote about numbers."
                  "Exercise 1.1: Evaluate the next chapter."
                  "Figure 1.1: A tree."]
                 (->> blocks (filter #(and (= :text (:kind %)) (= 1 (:page %)))) (map :text)))))
        (testing "no navigation nor script text; a document starts a page, a long one runs over"
          (is (not-any? #(re-find #"Next:|alert" (str (:text %))) blocks))
          (is (< 2 pages))
          (is (= (inc (:page (last (filter #(= :heading (:kind %)) blocks)))) (:page (last blocks))))))
      (let [html (epub/page-html (str f) #(str "/res/" %))
            p1   (first html)]
        (is (= (:pages (epub/extract (str f))) (count html)) "the same pages")
        (testing "fit to show inside the app"
          (is (re-find #"<img src=\"/res/OEBPS/img/f1.svg\"" p1) "an SVG object as an image, served apart")
          (is (re-find #"data-goto=\"2\"" p1) "a link inside the book goes to its page")
          (is (not (re-find #"onclick|<script|<nav|id=\"|>SVG<" (str/join html))))
          (is (re-find #"class=\"b-chapter\"" p1) "the book's classes, apart from the app's")
          (is (re-find #"<div class=\"note\">" p1))
          (is (not (re-find #"javascript" (peek html))))
          (is (re-find #"href=\"https://example.org\" target=\"_blank\"" (peek html)))))
      (is (= "image/svg+xml" (:type (epub/resource (str f) "OEBPS/img/f1.svg"))))
      (is (nil? (epub/resource (str f) "OEBPS/none.png")))
      (finally (.delete f)))))
