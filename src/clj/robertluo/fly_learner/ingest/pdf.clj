(ns robertluo.fly-learner.ingest.pdf
  "PDF source: extract text into a flat sequence of blocks.

    {:kind :heading :depth d :title \"1.1 System Fω\" :page p}
    {:kind :text    :text \"...\" :page p}

  Paragraphs come from PDFBox's paragraph detection. Page numbers are dropped,
  paragraphs split by a page break are rejoined, and runs of short fragments
  (formulas, figure bodies) are merged into one block. Headings are recognised
  from their text since many PDFs carry no outline."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.apache.pdfbox Loader)
           (org.apache.pdfbox.pdmodel PDDocument)
           (org.apache.pdfbox.text PDFTextStripper)))

(def ^:private para-mark " ")

(defn- page-paragraphs [^PDDocument doc ^long page]
  (let [stripper (doto (PDFTextStripper.)
                   (.setParagraphStart para-mark)
                   (.setStartPage page)
                   (.setEndPage page))]
    (->> (str/split (.getText stripper doc) (re-pattern para-mark))
         (map str/trim)
         (remove str/blank?))))

(defn clean-text
  "Join hard-wrapped lines and undo end-of-line hyphenation."
  [s]
  (-> s
      (str/replace #"(\p{Ll})-\s*\R\s*(\p{Ll})" "$1$2")
      (str/replace #"\s*\R\s*" " ")
      str/trim))

(defn- page-number? [s] (re-matches #"(?i)\d{1,4}|[ivxlc]{1,7}" s))

(defn heading
  "Return {:depth d :title t} when paragraph text `s` looks like a heading."
  [s]
  (let [s (clean-text s)]
    (cond
      (re-matches #"Chapter \d+" s)
      {:depth 1 :title s :chapter-marker true}

      (and (re-matches #"(\d+(?:\.\d+)+)\s+\p{Lu}.{0,80}" s)
           (not (re-find #"[.:;,]$|\. \. \." s)))
      {:depth (count (re-seq #"\d+" (first (str/split s #"\s")))) :title s}

      (re-matches #"[\p{Lu}][\p{Lu}\s\-]{2,40}" s)
      {:depth 1 :title s})))

(def ^:private minor-words #{"a" "an" "and" "as" "at" "by" "for" "in" "of" "on" "or" "the" "to"})

(defn- title-case
  "\"OBJECT NORMALIZATION AND φ THE FOIL\" -> \"Object Normalization and φ the Foil\".
  Titles that already contain lowercase Latin letters are left alone."
  [s]
  (if (re-find #"[a-z]" s)
    s
    (->> (str/split s #" ")
         (map-indexed (fn [i w]
                        (let [lw (str/lower-case w)]
                          (cond (not (re-matches #"[A-Z\-]+" w)) w
                                (and (pos? i) (minor-words lw)) lw
                                :else (str/capitalize lw)))))
         (str/join " "))))

(defn- merge-chapter-markers
  "\"Chapter 1\" followed by a short title line -> one heading \"1 Introduction\"."
  [blocks]
  (loop [bs (seq blocks), out []]
    (if-not bs
      out
      (let [[b nxt] bs]
        (if (and (:chapter-marker b) nxt (< (count (or (:title nxt) (:text nxt))) 100))
          (recur (nnext bs)
                 (conj out (-> b
                               (dissoc :chapter-marker)
                               (assoc :title (str (re-find #"\d+" (:title b)) " "
                                                  (title-case (or (:title nxt) (:text nxt))))))))
          (recur (next bs)
                 (conj out (cond-> (dissoc b :chapter-marker)
                             (= :heading (:kind b)) (update :title title-case)))))))))

(def ^:private fragment-chars 80)

(defn- continues?
  "Prose paragraph cut mid-sentence (by a page break or a display formula)."
  [prev next-text]
  (and (= :text (:kind prev))
       (>= (count (:text prev)) fragment-chars)
       (not (re-find #"[.!?:;)\]]$" (:text prev)))
       (re-find #"^\p{Ll}" next-text)))

(defn- fragment? [b]
  (and (= :text (:kind b)) (< (count (:text b)) fragment-chars)))

(defn- merge-fragments
  "Collapse runs of short text blocks (formulas, figure bodies) into one block."
  [blocks]
  (reduce (fn [out b]
            (let [prev (peek out)]
              (if (and prev (fragment? b) (= :text (:kind prev))
                       (or (:fragments prev) (fragment? prev)))
                (conj (pop out) (assoc prev
                                       :text (str (:text prev) "\n" (:text b))
                                       :fragments true))
                (conj out b))))
          [] blocks))

(defn blocks
  "Turn per-page paragraph strings [[page [para ...]] ...] into blocks."
  [pages]
  (->> pages
       (reduce (fn [out [page paras]]
                 (reduce (fn [out para]
                           (let [h    (heading para)
                                 text (clean-text para)
                                 prev (peek out)]
                             (cond
                               h (conj out (assoc h :kind :heading :page page))
                               ;; paragraph cut by a page break or a display formula
                               (and prev (continues? prev text))
                               (conj (pop out) (update prev :text str " " text))
                               :else (conj out {:kind :text :text text :page page}))))
                         out
                         (remove page-number? paras)))
               [])
       merge-chapter-markers
       merge-fragments
       (mapv #(dissoc % :fragments))))

(defn extract
  "Return {:title .. :pages n :blocks [...]}."
  [path]
  (with-open [doc (Loader/loadPDF (io/file path))]
    (let [n (.getNumberOfPages doc)]
      {:title  (or (some-> doc .getDocumentInformation .getTitle not-empty)
                   (.getName (io/file path)))
       :pages  n
       :blocks (blocks (for [p (range 1 (inc n))] [p (page-paragraphs doc p)]))})))
