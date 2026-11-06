(ns robertluo.fly-learner.ingest.pdf
  "PDF source: extract text into a flat sequence of blocks.

    {:kind :heading :depth d :title \"1.1 System Fω\" :page p}
    {:kind :text    :text \"...\" :page p}

  Paragraphs come from PDFBox's paragraph detection. Page numbers and running
  headers (a line on many pages) are dropped, paragraphs split by a page break
  are rejoined, and runs of short fragments (formulas, figure bodies) are
  merged into one block. Headings are recognised from their text since many
  PDFs carry no outline; a book whose chapter openings are not recognised
  gets its chapters from its numbered sections (2.1 belongs to chapter 2)."
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

      ;; not a contents line ("2.5 Games ....... 81")
      (and (re-matches #"(\d+(?:\.\d+)+)\.?\s+\p{Lu}.{0,80}" s)
           (not (re-find #"[.:;,]$|\. \. \.|\.{4,}|…" s)))
      {:depth (count (re-seq #"\d+" (first (str/split s #"\s"))))
       :title (str/replace s #"^(\d+(?:\.\d+)+)\.\s" "$1 ")}

      ;; a title in capitals, with at least one real word: figure labels
      ;; ("L R", "A N N" in a game tree) are not titles
      (and (re-matches #"[\p{Lu}][\p{Lu}\s\-]{2,40}" s)
           (re-find #"\p{Lu}{3,}" s))
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

(defn- chapter-number [title] (second (re-find #"^(\d+)\.\d" title)))

(defn- title-line?
  "A short line that reads like a title, not a sentence."
  [{:keys [kind text]}]
  (and (= :text kind) (<= 3 (count text) 80) (not (re-find #"[.!?:;,]$" text)) (re-find #"^\p{L}" text)))

(defn- chapter-title
  "The chapter title set just before a chapter's first section on `page`: the
  title-like lines at the end of `out`, from the last one starting with a
  capital (a part title above it is not the chapter's). Returns [title n]
  (n: lines taken from `out`), or nil."
  [out page]
  (let [lines (->> (rseq out) (take-while #(and (title-line? %) (= page (:page %)))) (take 3) reverse vec)
        start (last (keep-indexed (fn [i b] (when (re-find #"^\p{Lu}" (:text b)) i)) lines))]
    (when start
      [(str/join " " (map :text (subvec lines start))) (- (count lines) start)])))

(defn- add-chapters
  "Chapters for a book whose chapter openings were not recognised: before
  the first section N.x of a chapter N that has no heading, a chapter
  heading, titled from the lines just before it on that page (where books
  set the chapter title), else \"Chapter N\"."
  [blocks]
  (loop [bs (seq blocks)
         out []
         seen (into #{} (keep #(when (= 1 (:depth %)) (second (re-find #"^(\d+)\s" (:title %))))) blocks)]
    (if-let [[b & more] bs]
      (let [n (when (and (= :heading (:kind b)) (< 1 (:depth b))) (chapter-number (:title b)))]
        (if (and n (not (seen n)))
          (let [[title taken] (chapter-title out (:page b))]
            (recur more
                   (conj (subvec out 0 (- (count out) (or taken 0)))
                         {:kind :heading :depth 1 :page (:page b)
                          :title (str n " " (or title (str "Chapter " n)))}
                         b)
                   (conj seen n)))
          (recur more (conj out b) seen)))
      out)))

(def ^:private matter
  "Headings in capitals that stand in a numbered book: front and back matter, appendices."
  #"(?i)(abstract|public abstract|preface|foreword|introduction|contents|table of contents|acknowledge?ments|dedication|notation|glossary|bibliography|references|index|appendix.*|list of .*)")

(defn- numbered-section? [b]
  (and (= :heading (:kind b)) (< 1 (:depth b)) (chapter-number (:title b))))

(defn- demote-capitals
  "In a book structured by numbered sections, a line in capitals is a label
  (a player or a node in a game tree, a table head), not a heading, unless it
  is front or back matter."
  [blocks]
  (if (< (count (filter numbered-section? blocks)) 5)
    blocks
    (mapv (fn [{:keys [kind depth title] :as b}]
            (if (and (= :heading kind) (= 1 depth)
                     (not (re-find #"^\d" title))
                     (not (re-matches matter title)))
              {:kind :text :text title :page (:page b)}
              b))
          blocks)))

(defn- split-at-headings
  "A paragraph holding a numbered section heading among its lines (a chapter
  opening set as one block: part title, chapter title, \"1.1. Games\") cut
  into its lines, so the heading stands alone."
  [para]
  (let [lines (str/split-lines para)]
    (if (and (< 1 (count lines))
             (some #(and (re-find #"^\d+\.\d" (str/trim %)) (heading %)) lines))
      (map str/trim lines)
      [para])))

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

(defn running-lines
  "Lines on at least `share` of the pages (and on 3 at least): running headers
  and footers, as the book's title or its author on every page."
  ([pages] (running-lines pages 0.3))
  ([pages share]
   (let [per-page (for [[_ paras] pages]
                    (into #{} (comp (mapcat #(str/split-lines %)) (map str/trim)
                                    (filter #(<= 8 (count %) 100)))
                          paras))]
     (->> (frequencies (mapcat seq per-page))
          (filter (fn [[_ n]] (>= n (max 3 (* share (count pages))))))
          (map key)
          set))))

(defn contents-page?
  "A table of contents: most of its lines end with a page number."
  [paras]
  (let [lines (->> paras (mapcat str/split-lines) (map str/trim) (remove str/blank?))]
    (and (<= 5 (count lines))
         (<= 0.5 (/ (count (filter #(re-find #"\p{L}.*[\s.]\d{1,4}$" %) lines)) (count lines))))))

(defn- drop-lines
  "Paragraphs with the lines in `lines` removed (blank ones dropped)."
  [lines paras]
  (if (empty? lines)
    paras
    (->> paras
         (map (fn [p] (->> (str/split-lines p) (remove #(lines (str/trim %))) (str/join "\n"))))
         (remove str/blank?))))

(defn blocks
  "Turn per-page paragraph strings [[page [para ...]] ...] into blocks."
  [pages]
  (->> (let [running (running-lines pages)]
         (for [[page paras] pages :when (not (contents-page? paras))]
           [page (drop-lines running paras)]))
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
                         (remove page-number? (mapcat split-at-headings paras))))
               [])
       merge-chapter-markers
       demote-capitals
       add-chapters
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
