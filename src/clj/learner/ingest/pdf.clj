(ns learner.ingest.pdf
  "PDF source: extract text page by page and split it into paragraphs.
  Paragraphs become the leaf nodes of the knowledge DAG."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.apache.pdfbox Loader)
           (org.apache.pdfbox.pdmodel PDDocument)
           (org.apache.pdfbox.text PDFTextStripper)))

(defn- page-text [^PDDocument doc ^long page]
  (let [stripper (doto (PDFTextStripper.)
                   (.setSortByPosition true)
                   (.setStartPage page)
                   (.setEndPage page))]
    (.getText stripper doc)))

(defn split-paragraphs
  "Split page text on blank lines; join hard-wrapped lines inside a paragraph."
  [text]
  (->> (str/split text #"\R\s*\R")
       (map #(-> % (str/replace #"-\R(\p{L})" "$1") (str/replace #"\s*\R\s*" " ") str/trim))
       (remove str/blank?)))

(defn extract
  "Return {:title .. :pages n :paragraphs [{:page p :order i :text ..} ...]}."
  [path]
  (with-open [doc (Loader/loadPDF (io/file path))]
    (let [n     (.getNumberOfPages doc)
          title (or (some-> doc .getDocumentInformation .getTitle not-empty)
                    (.getName (io/file path)))]
      {:title      title
       :pages      n
       :paragraphs (->> (range 1 (inc n))
                        (mapcat (fn [p] (map #(hash-map :page p :text %)
                                             (split-paragraphs (page-text doc p)))))
                        (map-indexed (fn [i para] (assoc para :order i)))
                        vec)})))
