(ns robertluo.fly-learner.ingest.structure
  "What a document is made of, where its format does not say, judged by
  the :common model. Formats differ; so do languages and publishers, so
  nothing here knows any of them:

  - headings: a document whose text mostly stands under no heading (a plain
    text made into an EPUB: its chapter titles are lines like any other)
    has them found among its short lines (`needs-headings?`,
    `find-headings`, `with-headings`);
  - parts: each top part of a document (outline/parts) judged as the work's
    own content or not (a licence, a distributor's notes, contents, an
    index: `judge-parts`), so the learner is offered to leave those out.
    The learner decides; the judgement only sets what is offered.

  robertluo.fly-learner.ingest keeps both by content, so a document is
  judged once."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ingest.outline :as outline]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.prompts :as prompts]))

;;=============================================================================
;; Headings
;;=============================================================================

(defn- text-chars [blocks] (reduce + (map #(count (:text %)) (filter #(= :text (:kind %)) blocks))))

(defn needs-headings?
  "Does most of the text of `blocks` stand under no heading?"
  [blocks]
  (let [total (text-chars blocks)]
    (and (pos? total) (> (text-chars (take-while #(not= :heading (:kind %)) blocks)) (/ total 2)))))

^:rct/test
(comment
  (needs-headings? [{:kind :text :text "a long text with no heading"} {:kind :heading :title "License"} {:kind :text :text "b"}]) ;=> true
  (needs-headings? [{:kind :text :text "cover"} {:kind :heading :title "One"} {:kind :text :text "a long chapter text"}]) ;=> false
  )

(def ^:private line-chars "A line longer than this is no heading." 80)

(defn- lines
  "The lines of `blocks` a heading may be: [{:i index :text :heading depth
  :after chars}], the short one-line paragraphs and the headings already
  there, each with the characters of text after it before the next."
  [blocks]
  (let [ls (keep-indexed (fn [i {:keys [kind text title depth]}]
                           (cond (= :heading kind) {:i i :text title :heading depth}
                                 (and (<= (count text) line-chars) (not (str/includes? text "\n"))) {:i i :text text}))
                         blocks)
        ends (concat (map :i (rest ls)) [(count blocks)])]
    (mapv (fn [l end] (assoc l :after (text-chars (subvec (vec blocks) (inc (:i l)) end)))) ls ends)))

(def ^:private headings-system
  (prompts/prompt!
   :structure/headings
   {:group "Reading" :title "Find the headings"
    :about "On adding a document whose text mostly stands under no heading (a plain text made an EPUB, say): its short lines, a few hundred a call (common tier), and which are the headings of its parts and chapters. The reply is JSON."
    :default
    "You find the headings of a document whose format does not mark them: its
chapter titles are lines like any other. You get its short lines in reading
order, numbered, each with how many characters of text follow it before the
next line listed ([+n]). Lines marked [heading, depth d] are headings already
known.

Say which lines are headings of the document's divisions (parts, books,
chapters, sections) and their depth: 1 for its largest divisions, 2 for those
inside them, and so on, agreeing with the known headings. A heading stands
alone, usually with much text after it, and headings of one rank look alike
(a shared word, a numbering, a pattern). Not headings: a title page's lines
(title, author, translator, publisher, dates), verse, dialogue, a table's
cells, editorial notes such as \"(table omitted)\", lines inside the text.

Give each heading's title as it should be shown, in the document's own words
and script; a word every heading of its rank repeats (the work's name before
each chapter title) may be dropped when what remains still names it.
Reply with the headings only, in order."}))

(def ^:private headings-schema
  {"type"                 "object"
   "properties"           {"headings" {"type"  "array"
                                       "items" {"type"                 "object"
                                                "properties"           {"line"  {"type" "integer"}
                                                                        "depth" {"type" "integer"}
                                                                        "title" {"type" "string"}}
                                                "required"             ["line" "depth" "title"]
                                                "additionalProperties" false}}}
   "required"             ["headings"]
   "additionalProperties" false})

(def ^:private batch-lines 400)

(defn- headings-prompt [title batch found]
  (str "Document: " title "\n\n"
       (when (seq found)
         (str "Headings found before these lines (the last ones):\n"
              (str/join "\n" (for [{:keys [depth title]} (take-last 12 found)] (str "- depth " depth ": " title)))
              "\n\n"))
       "Lines:\n"
       (str/join "\n" (for [{:keys [n text heading after]} batch]
                        (str n ". " (when heading (str "[heading, depth " heading "] ")) text " [+" after "]")))))

(defn find-headings
  "The headings among the lines of `blocks` (of the document titled
  `title`), by `client`'s :common model (opts :models): {block-index
  {:depth :title}}, the headings already there left as they are."
  [client {:keys [models]} title blocks]
  (let [ls    (map-indexed #(assoc %2 :n (inc %1)) (lines blocks))
        by-n  (into {} (map (juxt :n identity)) ls)
        model (llm/model-for models :common)]
    (loop [[batch & more] (partition-all batch-lines ls), found []]
      (if-not batch
        (into {} (map (juxt :i #(select-keys % [:depth :title]))) found)
        (let [{:keys [headings]} (llm/complete-json client {:system     (prompts/text headings-system)
                                                            :prompt     (headings-prompt title batch found)
                                                            :model      model
                                                            :max-tokens 12000
                                                            :schema     headings-schema})
              in-batch (set (map :n batch))
              new      (for [{:keys [line depth title]} headings
                             :let [l (by-n line)]
                             :when (and l (in-batch line) (not (:heading l)) (pos-int? depth) (not (str/blank? title)))]
                         {:i (:i l) :depth depth :title (str/trim title)})]
          (recur more (into found new)))))))

(defn with-headings
  "`blocks` with those at the indexes of `found` ({index {:depth :title}},
  `find-headings`) made headings."
  [blocks found]
  (if (empty? found)
    (vec blocks)
    (vec (map-indexed (fn [i b]
                        (if-let [{:keys [depth title]} (found i)]
                          {:kind :heading :depth depth :title title :page (:page b)}
                          b))
                      blocks))))

^:rct/test
(comment
  (lines [{:kind :text :text "Chapter 1"} {:kind :text :text (apply str (repeat 100 "x"))}
          {:kind :heading :depth 1 :title "License"} {:kind :text :text "b"}])
  ;=> [{:i 0 :text "Chapter 1" :after 100} {:i 2 :text "License" :heading 1 :after 0} {:i 3 :text "b" :after 0}]
  (with-headings [{:kind :text :text "史記 五帝本紀" :page 3} {:kind :text :text "黃帝者" :page 3}] {0 {:depth 1 :title "五帝本紀"}})
  ;=> [{:kind :heading :depth 1 :title "五帝本紀" :page 3} {:kind :text :text "黃帝者" :page 3}]
  )

;;=============================================================================
;; Parts: the work's own, or not
;;=============================================================================

(def ^:private parts-system
  (prompts/prompt!
   :structure/parts
   {:group "Reading" :title "Spot what is not the work"
    :about "On adding a document: its top parts by title, size and opening, one call (common tier), and which are not the work's own content (a licence, a publisher's notes, an index). Those are offered unticked; you decide. The reply is JSON."
    :default
    "You help a learner choose what of a document to study. You get its title and
its top parts, each with its size and how it opens. Say of each whether it is
the work's own content, what a reader of it studies, or not: a licence or legal
notice, a distributor's, publisher's or digitiser's notes, an advertisement, a
title or copyright page, a table of contents, an index, a list of figures or
tables, acknowledgements, a dedication, a colophon, a bibliography or list of
references. Prefaces, introductions, appendices, notes and exercises are
content. When unsure, it is content.

For each part not content, say in a few words what it is (\"the
distributor's licence\"), in the language of the document."}))

(def ^:private parts-schema
  {"type"                 "object"
   "properties"           {"parts" {"type"  "array"
                                    "items" {"type"                 "object"
                                             "properties"           {"part"    {"type" "integer"}
                                                                     "content" {"type" "boolean"}
                                                                     "what"    {"type" "string"}}
                                             "required"             ["part" "content" "what"]
                                             "additionalProperties" false}}}
   "required"             ["parts"]
   "additionalProperties" false})

(def ^:private opening-chars 300)

(defn- part-text [part]
  (keep :text (tree-seq :children :children part)))

(defn- parts-prompt [title parts]
  (str "Document: " title "\n\nParts:\n\n"
       (str/join "\n\n"
                 (map-indexed (fn [i p]
                                (let [ts (part-text p), opening (str/join " " ts)]
                                  (str (inc i) ". " (:title p) " (" (reduce + (map count ts)) " characters)\n"
                                       "   " (subs opening 0 (min opening-chars (count opening))))))
                              parts))))

(defn judge-parts
  "The parts of section tree `tree` (outline/parts) that are not the work's
  own content, by `client`'s :common model (opts :models): {title what}."
  [client {:keys [models]} tree]
  (let [ps (outline/parts tree)]
    (if (< (count ps) 2)
      {}
      (let [{:keys [parts]} (llm/complete-json client {:system     (prompts/text parts-system)
                                                       :prompt     (parts-prompt (:title tree) ps)
                                                       :model      (llm/model-for models :common)
                                                       :max-tokens 4000
                                                       :schema     parts-schema})]
        (into {} (for [{:keys [part content what]} parts
                       :let [p (get ps (dec part))]
                       :when (and p (false? content))]
                   [(:title p) (str/trim (str what))]))))))
