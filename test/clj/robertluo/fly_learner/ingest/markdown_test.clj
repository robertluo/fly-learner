(ns robertluo.fly-learner.ingest.markdown-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [robertluo.fly-learner.ingest :as ingest]
            [robertluo.fly-learner.ingest.markdown :as md]))

(defn- folder!
  "A new folder (outside any git repository) holding `files` {path text}."
  [files]
  (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "fly-learner-md-" (random-uuid)))]
    (doseq [[p text] files]
      (let [f (io/file dir p)]
        (io/make-parents f)
        (if (bytes? text)
          (with-open [o (io/output-stream f)] (.write o ^bytes text))
          (spit f text))))
    dir))

(defn- delete! [^java.io.File dir]
  (doseq [^java.io.File f (reverse (file-seq dir))] (.delete f)))

(defn- headings [blocks]
  (map (juxt :depth :title) (filter #(= :heading (:kind %)) blocks)))

(def ^:private docs
  {"docs.json" "{\"navigation\": [
                  {\"title\": \"Start\", \"items\": [{\"title\": \"Overview\", \"path\": \"index.md\"},
                                                     {\"title\": \"Two tops\", \"path\": \"two.md\"}]},
                  {\"title\": \"Guides\", \"items\": [{\"title\": \"Run\", \"items\": [{\"path\": \"run.md\"}]}]},
                  {\"title\": \"Reference\", \"items\": [{\"path\": \"cli.md\"}]}],
                \"redirects\": [{\"from\": \"old.md\", \"to\": \"run.md\"}]}"
   "index.md"  "---\ntitle: Ignored\n---\n# Tool\n\nIntro, see [running](run.md#the-loop), [old](old.md), [cli](cli.md) and [code](../src/a.ts).\n\n![shot](images/a.png)\n\n## Why\n\nBecause.\n"
   "two.md"    "# First top\n\nOne.\n\n# Second top\n\nTwo.\n"
   "run.md"    "# Running\n\nRun it:\n\n```sh\ntool run\n```\n\n## The loop\n\nIt loops.\n\n## The loop\n\nAgain.\n"
   "cli.md"    "# CLI\n\nFlags.\n"
   "notes.md"  "# Notes\n\nNot listed.\n"
   "images/a.png" (byte-array [(unchecked-byte 0x89) 0x50 0x4e 0x47])})

(deftest a-folder-is-read-as-a-book
  (let [dir (folder! docs)]
    (try
      (let [{:keys [title pages blocks about]} (md/extract (str dir) {:skip ["reference"]})]
        (is (= :markdown (ingest/source-type (str dir))))
        (is (= "Tool" title) "titled by its first chapter")
        (testing "parts from the manifest before their first chapter; a chapter's headings under it"
          (is (= [[1 "Start"] [2 "Tool"] [3 "Why"]
                  [2 "Two tops"] [3 "First top"] [3 "Second top"]
                  [1 "Guides"] [2 "Run"] [3 "Running"] [4 "The loop"] [4 "The loop"]]
                 (headings blocks))))
        (testing "a file starts a page; code joins its paragraph"
          (is (= 3 pages))
          (is (some #(= "Run it:\ntool run" (:text %)) blocks)))
        (is (= {:manifest "docs.json" :chapters 3 :left-out ["Reference"] :unlisted ["notes.md"] :missing []}
               (dissoc about :repo))))
      (let [html (md/page-html (str dir) {:skip ["Reference"]} #(str "/res/" %))
            p1   (first html)]
        (testing "links inside the book go to their pages, by GitHub's anchors and the manifest's redirects"
          (is (re-find #"<a href=\"#page-3\" data-goto=\"3\">running</a>" p1))
          (is (re-find #"<a href=\"#page-3\" data-goto=\"3\">old</a>" p1))
          (is (re-find #"<a>cli</a>" p1) "a part left out, no repository to open it on: no link")
          (is (re-find #"<a>code</a>" p1)))
        (is (re-find #"<img src=\"/res/images/a.png\"" p1))
        (is (not (re-find #"title: Ignored|id=\"" (str/join html)))))
      (testing "images inside the folder only"
        (is (= "image/png" (:type (md/resource (str dir) "images/a.png"))))
        (is (nil? (md/resource (str dir) "index.md")))
        (is (nil? (md/resource (str dir) "../../etc/hosts"))))
      (testing "the book's hash: its chapters and what is left out, not other files"
        (let [h (md/content-hash (str dir) {:skip ["Reference"]})]
          (spit (io/file dir "notes.md") "# Notes\n\nChanged.\n")
          (is (= h (md/content-hash (str dir) {:skip ["Reference"]})))
          (is (not= h (md/content-hash (str dir) {})))
          (spit (io/file dir "run.md") "# Running\n\nChanged.\n")
          (is (not= h (md/content-hash (str dir) {:skip ["Reference"]})))))
      (finally (delete! dir)))))

(deftest front-matter-and-formulas
  (let [dir (folder! {"intro.md"   (str "---\ntitle: Getting started\ndescription: What the tool is for.\n"
                                       "tags: [agents, terminal]\nauthors: [Ann]\ndate: 2024-05-01\nsidebar_position: 2\n---\n"
                                       "Plain text without a heading, $x^2$ inline.\n\n$$\n\\int_0^1 x\\,dx\n$$\n\nCosts $5 or $10.\n")
                      "a-first.md" "---\nweight: 1\n---\n# First\n\n```math\na^2\n```\n"
                      "draft.md"   "---\ndraft: true\n---\n# Draft\n\nNot yet.\n"
                      "z-last.md"  "+++\ntitle = \"Last\"\n+++\nNo heading, TOML front matter.\n"
                      "guide/_category_.json" "{\"label\": \"The guide\", \"position\": 0}"
                      "guide/one.md" "# One\n\nOne.\n"})]
    (try
      (let [{:keys [blocks about]} (md/extract (str dir))
            tree (ingest/outline (str dir))]
        (testing "the place front matter gives; a folder's _category_; drafts left out"
          (is (= [[1 "The guide"] [2 "One"] [1 "First"] [1 "Getting started"] [1 "Last"]] (headings blocks)))
          (is (= ["draft.md (draft)"] (:left-out about))))
        (testing "the description is the chapter's first passage; tags, authors, date are not read"
          (is (= ["What the tool is for."
                  "Plain text without a heading, $x^2$ inline.\n$$\\int_0^1 x\\,dx$$"
                  "Costs $5 or $10."]
                 (->> blocks (drop-while #(not= "Getting started" (:title %))) rest
                      (take-while :text) (map :text)))))
        (testing "a chapter's tags are its keywords"
          (is (= ["agents" "terminal"] (:tags (first (filter #(= "Getting started" (:title %)) (:children tree))))))))
      (let [html (str/join (md/page-html (str dir) {} identity))]
        (is (re-find #"<p data-lead=\"\">What the tool is for.</p>" html))
        (is (re-find #"<span data-tag=\"\">agents</span>" html))
        (is (re-find #"<span data-byline=\"\">Ann · 2024-05-01</span>" html))
        (is (re-find #"<span data-tex=\"inline\">\$x\^2\$</span>" html))
        (is (re-find #"<div data-tex=\"display\">\$\$a\^2\$\$</div>" html) "a ```math fence")
        (is (not (re-find #"title:|weight|\+\+\+" html)) "no front matter shown as text"))
      (finally (delete! dir)))))

(deftest other-manifests
  (testing "SUMMARY.md: headings are parts, nested lists chapters"
    (let [dir (folder! {"SUMMARY.md" "# Summary\n\n[Intro](intro.md)\n\n# Basics\n\n- [One](one.md)\n  - [Two](sub/two.md)\n"
                        "intro.md"   "# Intro\n\nHi.\n"
                        "one.md"     "One has no heading.\n"
                        "sub/two.md" "# Two\n\nText.\n"})]
      (try
        (let [{:keys [blocks about]} (md/extract (str dir))]
          (is (= [[1 "Intro"] [1 "Basics"] [2 "One"] [3 "Two"]] (headings blocks))
              "a chapter without a heading is titled by the manifest")
          (is (= "SUMMARY.md" (:manifest about))))
        (finally (delete! dir)))))
  (testing "no manifest: the index, files in natural order, folders as parts"
    (let [dir (folder! {"README.md"         "# Guide\n\nStart.\n"
                        "10-last.md"        "# Last\n\nEnd.\n"
                        "2-next.md"         "# Next\n\nThen.\n"
                        "CHANGELOG.md"      "# Changes\n\n1.0\n"
                        "deep-dive/one.md"  "# Deep one\n\nDeep.\n"})]
      (try
        (let [{:keys [blocks about]} (md/extract (str dir))]
          (is (= [[1 "Guide"] [1 "Next"] [1 "Last"] [1 "Deep dive"] [2 "Deep one"]] (headings blocks)))
          (is (nil? (:manifest about)))
          (is (= [] (:unlisted about)) "the project's own files are no chapters"))
        (finally (delete! dir))))))
