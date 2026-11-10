(ns robertluo.fly-learner.ingest.markdown
  "A folder of Markdown files as a book (robertluo.fly-learner.ingest.book):
  a repository's docs, say, or one Markdown file. Each file is a chapter,
  rendered to HTML (CommonMark with GitHub's tables, strikethrough, autolinks,
  task lists, footnotes and TeX formulas, robertluo.fly-learner.ingest.tex)
  and read as pages as an EPUB's documents are.

  A file's front matter (YAML between --- lines, Hugo's TOML between +++)
  says more of it (`fields`): its title, a description (shown under the
  chapter's heading and read as its first passage), tags (shown, and the
  chapter's keywords), authors and date (shown), its place in the folder's
  order (sidebar_position, nav_order, weight, ..) and whether it is a draft
  (left out).

  The reading order and the parts come from the folder's manifest, the
  first of:
    docs.json, mint.json  a JSON navigation tree: entries with a title and a
                          file (`path`, `page`, ..) or entries of their own
                          (`items`, `pages`, `groups`, ..); `redirects`
    SUMMARY.md            mdBook / GitBook: headings are parts, nested lists
                          of links the chapters (in the folder or its src/)
    the folder itself     index.md / README.md / _index.md first, then the
                          files and folders by their place (front matter,
                          a folder's _category_.json|yml or index), the rest
                          files first, in natural order (\"2-x\" before
                          \"10-y\"); a folder is a part (its own index
                          opening it, its _category_ label naming it)
  A group's title is a part heading put before its first chapter; parts the
  learner leaves out (`:skip`: titles or file names, any case) are not in
  the book. A chapter's headings nest under it: a file with one top heading
  first is titled by it, else by its front matter's title, its title in the
  manifest or its file name.

  Names are paths from the top of the git repository holding the folder
  (when the folder is part of it, not ignored by it), else from the folder:
  a link to a chapter goes to its page (anchors as GitHub makes them from
  headings), a link to anything else in the repository opens it on GitHub at
  the commit checked out, and images are served from inside the repository
  only (`resource`)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clj-yaml.core :as yaml]
            [jsonista.core :as json]
            [robertluo.fly-learner.ingest.book :as book]
            [robertluo.fly-learner.ingest.tex :as tex])
  (:import (java.io File)
           (java.security MessageDigest)
           (org.commonmark.ext.autolink AutolinkExtension)
           (org.commonmark.ext.footnotes FootnotesExtension)
           (org.commonmark.ext.gfm.strikethrough StrikethroughExtension)
           (org.commonmark.ext.gfm.tables TablesExtension)
           (org.commonmark.ext.task.list.items TaskListItemsExtension)
           (org.commonmark.parser Parser)
           (org.commonmark.renderer.html HtmlRenderer)
           (org.jsoup Jsoup)
           (org.jsoup.nodes Document Element)))

;;=============================================================================
;; Markdown -> HTML
;;=============================================================================

(def ^:private extensions
  [(TablesExtension/create) (StrikethroughExtension/create) (AutolinkExtension/create)
   (TaskListItemsExtension/create) (FootnotesExtension/create)
   tex/parser-extension tex/renderer-extension])

(def ^:private parser (.build (.extensions (Parser/builder) extensions)))
(def ^:private renderer (.build (.extensions (HtmlRenderer/builder) extensions)))

(defn- toml-map
  "The top-level keys of Hugo's TOML front matter `s` (strings, numbers,
  booleans, arrays of strings), up to its first table."
  [s]
  (let [unquote #(or (second (re-matches #"\"(.*)\"" %)) (second (re-matches #"'(.*)'" %)))]
    (into {} (for [line (take-while #(not (re-matches #"\s*\[.*\]\s*" %)) (str/split-lines s))
                   :let [[_ k v] (re-matches #"\s*([A-Za-z_][\w-]*)\s*=\s*(.+?)\s*" line)]
                   :when k]
               [k (cond (unquote v)                     (unquote v)
                        (re-matches #"-?\d+(\.\d+)?" v) (parse-double v)
                        (= "true" v)                    true
                        (= "false" v)                   false
                        (re-matches #"\[.*\]" v)        (vec (keep #(or (second %) (nth % 2))
                                                                   (re-seq #"\"([^\"]*)\"|'([^']*)'" v)))
                        :else                           v)]))))

(defn front-matter
  "[fields body] of Markdown text `s`: its front matter (YAML between ---
  lines, or Hugo's TOML between +++ lines) as a map with string keys, and
  the text after it; [{} s] when it has none. YAML is read safely (no tags
  making objects); front matter that is no YAML is dropped all the same."
  [s]
  (let [s (str/replace-first s #"^\uFEFF" "")]
    (or (when-let [[whole fm] (re-find #"^---[ \t]*\r?\n(?:([\s\S]*?)\r?\n)?(?:---|\.\.\.)[ \t]*(?:\r?\n|$)" s)]
          (let [m (if (str/blank? fm)
                    {}
                    (try (yaml/parse-string fm :keywords false :unknown-tag-fn :value)
                         (catch Exception _ {})))]
            (when (or (map? m) (nil? m))
              [(or m {}) (subs s (count whole))])))
        (when-let [[whole fm] (re-find #"^\+\+\+[ \t]*\r?\n(?:([\s\S]*?)\r?\n)?\+\+\+[ \t]*(?:\r?\n|$)" s)]
          [(toml-map (str fm)) (subs s (count whole))])
        [{} s])))

^:rct/test
(comment
  (front-matter "---\ntitle: \"Hello\"\ntags: [a, b]\n---\n# A\n") ;=> [{"title" "Hello" "tags" ["a" "b"]} "# A\n"]
  (front-matter "+++\ntitle = 'Hi'\nweight = 3\ntags = [\"x\"]\n[params]\ny = 1\n+++\nText") ;=> [{"title" "Hi" "weight" 3.0 "tags" ["x"]} "Text"]
  (front-matter "---\n---\nBody") ;=> [{} "Body"]
  (front-matter "# A\n---\n") ;=> [{} "# A\n---\n"]
  (front-matter "---\nJust a line of prose\n---\nB") ;=> [{} "---\nJust a line of prose\n---\nB"]
  )

(defn- strings [v]
  (cond (string? v)     (remove str/blank? (map str/trim (str/split v #",")))
        (sequential? v) (keep #(cond (string? %) (not-empty (str/trim %))
                                     (number? %) (str %)
                                     (map? %)    (some-> (get % "name") str))
                              v)
        :else           []))

(defn- date-text [v]
  (cond (instance? java.util.Date v)
        (.format (doto (java.text.SimpleDateFormat. "yyyy-MM-dd") (.setTimeZone (java.util.TimeZone/getTimeZone "UTC"))) v)
        (string? v) (not-empty (str/trim v))))

(defn fields
  "What front matter `m` (`front-matter`) tells of its chapter, those it
  has: {:title :description :tags [..] :authors [..] :date \"2024-01-02\"
  :order n :draft? bool}. The names of Docusaurus, Jekyll, Hugo, MkDocs and
  Starlight."
  [m]
  (let [text  (fn [ks] (some #(let [v (get m %)] (when (and (string? v) (not (str/blank? v))) (str/trim v))) ks))
        num   #(cond (number? %) % (string? %) (parse-double (str/trim %)))
        order (or (some #(num (get m %)) ["sidebar_position" "nav_order" "weight" "order" "position"])
                  (let [sb (get m "sidebar")] (when (map? sb) (num (get sb "order")))))
        tags  (vec (distinct (mapcat #(strings (get m %)) ["tags" "keywords"])))
        by    (vec (mapcat #(strings (get m %)) ["authors" "author"]))
        date  (some #(date-text (get m %)) ["date" "published"])]
    (cond-> {}
      (text ["title"])                                           (assoc :title (text ["title"]))
      (text ["description" "summary" "abstract" "excerpt"])      (assoc :description (text ["description" "summary" "abstract" "excerpt"]))
      (seq tags)                                                 (assoc :tags tags)
      (seq by)                                                   (assoc :authors by)
      date                                                       (assoc :date date)
      order                                                      (assoc :order order)
      (or (true? (get m "draft")) (false? (get m "published"))) (assoc :draft? true))))

^:rct/test
(comment
  (fields {"title" "T" "tags" "a, b" "keywords" ["b" "c"] "sidebar_position" 2 "draft" true
           "authors" [{"name" "Ann"} "Bo"] "date" #inst "2024-01-02"})
  ;=> {:title "T" :tags ["a" "b" "c"] :authors ["Ann" "Bo"] :date "2024-01-02" :order 2 :draft? true}
  (fields {"sidebar" {"order" 3} "summary" "S"}) ;=> {:description "S" :order 3}
  )

(defn- file-fields [^File f]
  (fields (first (front-matter (slurp f :encoding "UTF-8")))))

(defn- html-doc ^Document [^String md]
  (tex/math-fences! (Jsoup/parse ^String (.render renderer (.parse parser md)))))

(defn slug
  "The anchor GitHub gives a heading with text `s`."
  [s]
  (-> (str/lower-case s)
      (str/replace #"[^\p{L}\p{M}\p{N}_\- ]" "")
      (str/replace " " "-")))

^:rct/test
(comment
  (slug "Invocation and Output") ;=> "invocation-and-output"
  (slug "`pi --mode rpc`: the protocol!") ;=> "pi---mode-rpc-the-protocol"
  )

(defn- headings [^Document doc] (vec (.select doc "h1, h2, h3, h4, h5, h6")))

(defn- level [^Element h] (parse-long (subs (.tagName h) 1)))

(defn- name-headings!
  "Give each heading of `doc` without an id the one GitHub would (repeats
  numbered: x, x-1, x-2)."
  [^Document doc]
  (reduce (fn [seen ^Element h]
            (if (seq (.id h))
              seen
              (let [s (slug (.text h)), n (seen s 0)]
                (.attr h "id" ^String (if (zero? n) s (str s "-" n)))
                (assoc seen s (inc n)))))
          {} (headings doc))
  doc)

;;=============================================================================
;; The folder: manifest, reading order
;;=============================================================================

(defn- md-file? [^File f]
  (and (.isFile f) (re-find #"(?i)\.(md|markdown)$" (.getName f))))

(def ^:private not-chapters
  "Files and folders that are about the project, not its subject."
  #"(?i)(changelog|changes|history|contributing|code_of_conduct|license|licence|authors|security|summary)(\.(md|markdown))?|node_modules|vendor|i18n|translations?|\..*")

(defn- natural-compare
  "Compare names with their runs of digits as numbers: \"2-x\" before \"10-y\"."
  [a b]
  (let [parts (fn [s] (re-seq #"\d+|\D+" (str/lower-case s)))]
    (loop [[x & xs :as as] (parts a), [y & ys :as bs] (parts b)]
      (cond (and (empty? as) (empty? bs)) 0
            (empty? as) -1
            (empty? bs) 1
            :else (let [c (if (and (re-matches #"\d+" x) (re-matches #"\d+" y))
                            (compare (parse-long x) (parse-long y))
                            (compare x y))]
                    (if (zero? c) (recur xs ys) c))))))

^:rct/test
(comment
  (vec (sort natural-compare ["10-end.md" "2-middle.md" "1-start.md" "b.md" "A.md"])) ;=> ["1-start.md" "2-middle.md" "10-end.md" "A.md" "b.md"]
  )

(defn- index-file? [^File f] (re-matches #"(?i)_?(index|readme)\.(md|markdown)" (.getName f)))

(defn- index-of
  "The Markdown file opening folder `dir`, if any."
  ^File [^File dir]
  (->> (.listFiles dir) (filter #(and (md-file? %) (index-file? %)))
       (sort-by #(.getName ^File %) natural-compare) first))

(defn- category
  "A folder's Docusaurus _category_ file: {:title :order}, those it has."
  [^File dir]
  (let [m (some (fn [n] (let [f (io/file dir n)]
                          (when (.isFile f)
                            (try (if (str/ends-with? n ".json")
                                   (json/read-value f)
                                   (yaml/parse-string (slurp f) :keywords false :unknown-tag-fn :value))
                                 (catch Exception _ nil)))))
                ["_category_.json" "_category_.yml" "_category_.yaml"])]
    (when (map? m)
      (cond-> {}
        (string? (get m "label"))    (assoc :title (get m "label"))
        (number? (get m "position")) (assoc :order (get m "position"))))))

(defn- by-place
  "Entries ranked: those with a place first, by it; then files before
  folders; then by name, naturally."
  [a b]
  (let [c (compare [(nil? (:order a)) (or (:order a) 0) (boolean (:folder? a))]
                   [(nil? (:order b)) (or (:order b) 0) (boolean (:folder? b))])]
    (if (zero? c) (natural-compare (:sort-name a) (:sort-name b)) c)))

(defn- folder-entries
  "Entries ({:title :file :items}) of the folder `dir`, files relative to
  `top`: its index first, then its files and folders (parts) by place."
  [^File dir ^File top]
  (let [rel   #(str (.relativize (.toPath top) (.toPath ^File %)))
        index (index-of dir)
        kids  (remove #(or (= index %) (re-matches not-chapters (.getName ^File %))) (.listFiles dir))
        entry (fn [^File k]
                (cond
                  (md-file? k)
                  {:file (rel k) :order (:order (file-fields k)) :sort-name (.getName k)}

                  (.isDirectory k)
                  (let [sub (folder-entries k top)]
                    (when (seq sub)
                      (let [idx (index-of k)
                            {:keys [title order]} (category k)]
                        (assoc (if idx
                                 (cond-> (assoc (first sub) :items (vec (rest sub))) title (assoc :title title))
                                 {:title (or title (str/capitalize (str/trim (str/replace (.getName k) #"^\d+[-_. ]*|[-_]" " "))))
                                  :items (vec sub)})
                               :order (or order (some-> idx file-fields :order))
                               :folder? true :sort-name (.getName k)))))))]
    (concat (when index [{:file (rel index)}])
            (->> (keep entry kids) (sort by-place) (map #(dissoc % :order :folder? :sort-name))))))

(def ^:private title-keys ["title" "group" "label" "name" "text"])
(def ^:private file-keys ["path" "page" "file" "href" "link" "source"])
(def ^:private items-keys ["items" "pages" "children" "groups" "sections" "chapters" "tabs" "anchors"])

(defn- json-entries
  "Entries of a JSON navigation tree `x` (strings may name files)."
  [x]
  (letfn [(pick [m ks pred] (some #(let [v (get m %)] (when (pred v) v)) ks))]
    (cond (string? x)     [{:file x}]
          (sequential? x) (vec (mapcat json-entries x))
          (map? x)        (let [title (pick x title-keys string?)
                                file  (pick x file-keys string?)
                                items (pick x items-keys sequential?)]
                            (if (or file items)
                              [(cond-> {:title title}
                                 file  (assoc :file file)
                                 items (assoc :items (json-entries items)))]
                              []))
          :else           [])))

(defn- json-manifest [^File f]
  (let [root (json/read-value f)
        nav  (if (map? root) (some #(get root %) ["navigation" "nav" "sidebar" "toc"]) root)]
    {:title     (when (map? root) (some #(let [v (get root %)] (when (string? v) v)) ["title" "name"]))
     :entries   (json-entries (or nav root))
     :redirects (into {} (for [r (when (map? root) (get root "redirects"))
                               :let [from (or (get r "from") (get r "source")), to (or (get r "to") (get r "destination"))]
                               :when (and (string? from) (string? to))]
                           [from to]))}))

(defn- summary-entries
  "Entries of an mdBook / GitBook SUMMARY.md: headings are parts, list items
  linking files the chapters (nested lists their sections)."
  [^File f]
  (let [doc (html-doc (second (front-matter (slurp f))))]
    (letfn [(item [^Element li]
              (let [a     (first (filter #(= "a" (.tagName ^Element %))
                                         (concat (.children li) (mapcat #(.children ^Element %) (.select li "> p")))))
                    title (str/trim (if a (.text ^Element a) (.ownText li)))
                    href  (some-> a (.attr "href") not-empty)
                    items (vec (mapcat list-items (.select li "> ul, > ol")))]
                [(cond-> {:title title} href (assoc :file href) (seq items) (assoc :items items))]))
            (list-items [^Element l] (mapcat item (.select l "> li")))]
      (->> (.children (.body doc))
           (reduce (fn [out ^Element e]
                     (let [t (.tagName e)]
                       (cond
                         (re-matches #"h[1-6]" t)
                         (if (re-matches #"(?i)summary|table of contents|contents" (.text e))
                           out
                           (conj out {:title (.text e) :items []}))

                         (#{"ul" "ol" "p"} t)
                         (let [new (if (= "p" t)
                                     (for [^Element a (.select e "a[href]")] {:title (.text a) :file (.attr a "href")})
                                     (list-items e))]
                           (if (and (seq out) (:items (peek out)) (not (:file (peek out))))
                             (conj (pop out) (update (peek out) :items into new))
                             (into out new)))

                         :else out)))
                   [])))))

(defn- manifest
  "The reading order of the folder `dir`: {:name (the manifest file, nil for
  the folder's own order) :dir (where its paths start) :title :entries
  :redirects}."
  [^File dir]
  (or (some (fn [n] (let [f (io/file dir n)]
                      (when (.isFile f)
                        (let [m (json-manifest f)]
                          (when (seq (:entries m)) (assoc m :name n :dir dir)))))) ["docs.json" "mint.json"])
      (some (fn [^File d] (let [f (io/file d "SUMMARY.md")]
                            (when (.isFile f)
                              (let [es (summary-entries f)]
                                (when (seq es) {:name (str (.relativize (.toPath dir) (.toPath f))) :dir d :entries es})))))
            [dir (io/file dir "src")])
      {:name nil :dir dir :entries (vec (folder-entries dir dir))}))

(defn- left-out? [skip {:keys [title file]}]
  (some (fn [s] (let [s (str/lower-case (str/trim s))]
                  (or (= s (some-> title str/trim str/lower-case))
                      (and file (or (= s (str/lower-case file))
                                    (= s (str/lower-case (.getName (io/file file)))))))))
        skip))

(defn- events
  "The entries in reading order as [:part title depth] and [:file file depth
  title]; those left out (`skip`) collected in `out` with what they hold."
  [entries depth skip out]
  (mapcat (fn [{:keys [title file items] :as e}]
            (cond (left-out? skip e)           (do (vswap! out conj e) [])
                  (and (nil? title) (nil? file)) (events items depth skip out)
                  :else (concat (if file [[:file file depth title]] [[:part title depth]])
                                (events items (inc depth) skip out))))
          entries))

(defn- files-of
  "The files `entries` name, in order."
  [entries]
  (mapcat #(cond->> (files-of (:items %)) (:file %) (cons (:file %))) entries))

(defn- git [dir & args]
  (try (let [{:keys [exit out]} (apply sh/sh "git" "-C" (str dir) args)]
         (when (zero? exit) (not-empty (str/trim out))))
       (catch java.io.IOException _ nil)))

(defn github-url
  "The web address of the GitHub repository of git remote `remote`, nil
  when not on GitHub."
  [remote]
  (when-let [[_ owner repo] (re-find #"github\.com[:/]([^/\s]+)/([^/\s]+?)(?:\.git)?/?$" (str/trim (str remote)))]
    (str "https://github.com/" owner "/" repo)))

^:rct/test
(comment
  (github-url "https://github.com/earendil-works/pi.git") ;=> "https://github.com/earendil-works/pi"
  (github-url "git@github.com:robertluo/fly-learner.git") ;=> "https://github.com/robertluo/fly-learner"
  (github-url "https://gitlab.com/a/b.git") ;=> nil
  )

(defn- repository
  "The git repository holding folder `dir`, when the folder is part of it
  (not ignored by it, as data/sources is by this project's):
  {:root File :web url-or-nil :commit sha}."
  [^File dir]
  (when-let [top (git dir "rev-parse" "--show-toplevel")]
    (let [top (.getCanonicalFile (io/file top))]
      (when-not (try (zero? (:exit (sh/sh "git" "-C" (str top) "check-ignore" "-q" (.getCanonicalPath dir))))
                     (catch java.io.IOException _ false))
        {:root   top
         :web    (some-> (git top "config" "--get" "remote.origin.url") github-url)
         :commit (git top "rev-parse" "HEAD")}))))

(defn- layout
  "What the book at `path` (a folder, or one Markdown file) holds, before
  reading it: {:dir :base (names start there) :repo :manifest :title :parts
  (the top entries of the manifest: {:title :skip}, what `:skip` takes)
  :chapters [{:file File :name :depth :title :parts [[title depth] ..]}]
  :redirects {name name} :left-out [title ..] :unlisted [file ..] :missing
  [file ..]}. opts: :skip, parts or files left out. Drafts are left out."
  [path {:keys [skip]}]
  (let [f      (.getCanonicalFile (io/file (str path)))
        dir    (if (.isDirectory f) f (.getParentFile f))
        repo   (repository dir)
        base   (or (:root repo) dir)
        name   #(str/replace (str (.relativize (.toPath base) (.toPath (.getCanonicalFile ^File %)))) File/separator "/")
        {mdir :dir :as m} (if (.isDirectory f)
                            (manifest dir)
                            {:dir dir :entries [{:file (.getName f)}]})
        out    (volatile! [])
        evs    (doall (events (:entries m) 1 skip out))
        file   (fn [s] (let [[p] (book/resolve-path "x" s)] (io/file mdir p)))
        drafts (volatile! [])
        [chapters _ seen missing]
        (reduce (fn [[cs parts seen missing] [kind x depth title]]
                  (case kind
                    :part [cs (conj (filterv #(< (second %) depth) parts) [x depth]) seen missing]
                    :file (let [^File f (file x)]
                            (cond (not (md-file? f))          [cs parts seen (conj missing x)]
                                  (seen (name f))             [cs parts seen missing]
                                  (:draft? (file-fields f))   (do (vswap! drafts conj (name f)) [cs parts seen missing])
                                  :else [(conj cs {:file f :name (name f) :depth depth :title title :parts parts})
                                         [] (conj seen (name f)) missing]))))
                [[] [] #{} []]
                evs)
        dropped (into (set (map #(name (file %)) (files-of @out))) @drafts)]
    {:dir       dir
     :base      base
     :repo      repo
     :manifest  (:name m)
     :title     (:title m)
     :chapters  chapters
     :parts     (let [ps (keep (fn [{:keys [title file]}]
                                 (when-let [k (or title file)]
                                   {:title (or title (last (str/split file #"/"))) :skip k}))
                               (:entries m))]
                  (when (< 1 (count ps)) (vec ps)))
     :redirects (into {} (for [[from to] (:redirects m)] [(name (file from)) (name (file to))]))
     :left-out  (into (vec (keep :title @out)) (map #(str (last (str/split % #"/")) " (draft)") @drafts))
     :missing   missing
     :unlisted  (when (.isDirectory f)
                  (->> (folder-entries dir dir) files-of (map #(name (io/file dir %)))
                       (remove #(or (seen %) (dropped %))) sort vec))}))

;;=============================================================================
;; The book
;;=============================================================================

(defn- heading!
  "Put a heading before everything in `doc`; returns it."
  ^Element [^Document doc tag id text]
  (let [h (doto (Element. ^String tag) (.attr "id" ^String id) (.text ^String text))]
    (.prependChild (.body doc) h)
    h))

(defn- front-matter!
  "Show what front matter `fs` (`fields`) says under the chapter heading `h`:
  the description as a lead paragraph (read: the chapter's first passage),
  then tags, authors and date, shown only (data-aside). The tags are the
  heading's keywords."
  [^Element h {:keys [description tags authors date]}]
  (when (or (seq tags) (seq authors) date)
    (let [p  (doto (Element. "p") (.attr "data-aside" "front-matter"))
          by (str/join " · " (remove nil? [(when (seq authors) (str/join ", " authors)) date]))]
      (doseq [t tags] (doto (.appendElement p "span") (.attr "data-tag" "") (.text ^String t)))
      (when (seq by) (doto (.appendElement p "span") (.attr "data-byline" "") (.text by)))
      (.after h ^org.jsoup.nodes.Node p)))
  (when description
    (.after h ^org.jsoup.nodes.Node (doto (Element. "p") (.attr "data-lead" "") (.text ^String description))))
  (when (seq tags)
    (.attr h "data-keywords" ^String (str/join "," tags))))

(defn- chapter
  "Chapter `c` of a layout as [name document toc title]: its parts' headings
  put before it, what its front matter says under its heading, every
  heading listed in the toc with its depth."
  [{:keys [^File file name depth title parts]}]
  (let [[fm md] (front-matter (slurp file :encoding "UTF-8"))
        fs      (fields fm)
        doc     (name-headings! (html-doc md))
        hs      (headings doc)
        levels  (vec (sort (distinct (map level hs))))
        top     (first levels)
        titled  (and (seq hs) (= top (level (first hs))) (= 1 (count (filter #(= top (level %)) hs))))
        offset  (if titled 0 1)
        toc     (into {} (for [h hs] [[name (.id ^Element h)] (+ depth offset (.indexOf levels (level h)))]))
        head    (if titled
                  (first hs)
                  (heading! doc "h1" "fly-chapter"
                            (str/trim (or (:title fs) title
                                          (str/capitalize (str/replace (.getName file) #"\.(md|markdown)$|[-_]" " "))))))
        toc     (cond-> toc (not titled) (assoc [name "fly-chapter"] depth))
        toc     (reduce (fn [toc [i [_ d]]] (assoc toc [name (str "fly-part-" i)] d))
                        toc (map-indexed vector parts))]
    (front-matter! head fs)
    ;; parts in order before the chapter: prepended last first
    (doseq [[i [t d]] (reverse (map-indexed vector parts))]
      (heading! doc (str "h" (min 6 d)) (str "fly-part-" i) t))
    [name doc toc (.text ^Element head)]))

(defn- package
  "The book at `path` as a package for robertluo.fly-learner.ingest.book,
  with its layout."
  [path opts]
  (let [{:keys [dir repo chapters redirects] :as l} (layout path opts)
        read (mapv chapter chapters)
        {:keys [web commit]} repo]
    {:layout    l
     :title     (or (:title l) (some #(nth % 3) (take 1 read)) (.getName ^File dir))
     :docs      (mapv (fn [[n d]] [n d]) read)
     :toc       (apply merge (map #(nth % 2) read))
     :redirects redirects
     :outside   (fn [name frag]
                  (when (and web commit (not (str/starts-with? name "..")))
                    (str web "/blob/" commit "/" (str/replace name " " "%20") (when frag (str "#" frag)))))}))

(defn extract
  "Return {:title .. :pages n :blocks [...]} (as robertluo.fly-learner.ingest.pdf/extract)
  and :about the folder {:manifest :chapters n :left-out :unlisted :missing
  :repo}. opts: :skip, parts or files left out."
  ([path] (extract path {}))
  ([path opts]
   (let [{:keys [layout] :as pkg} (package path opts)]
     (assoc (book/extract pkg)
            :about (-> (select-keys layout [:manifest :left-out :unlisted :missing :parts])
                       (assoc :chapters (count (:chapters layout))
                              :repo (some-> (:repo layout) (update :root str))))))))

(defn page-html
  "The book at `path` as pages of HTML to show: [html ..], page p at p - 1.
  `resource-url`: a name (an image) -> the URL it is served at."
  [path opts resource-url]
  (book/page-html (package path opts) resource-url))

(defn content-hash
  "SHA-256 (hex) of what the book at `path` is made of: its manifest, its
  chapters' names and bytes, the parts left out. A change elsewhere in the
  repository is no change of the book."
  [path {:keys [skip] :as opts}]
  (let [{:keys [dir manifest chapters]} (layout path opts)
        md (MessageDigest/getInstance "SHA-256")
        add! (fn [^bytes bs] (.update md bs))]
    (when manifest (add! (.getBytes ^String manifest "UTF-8")) (add! (java.nio.file.Files/readAllBytes (.toPath (io/file dir manifest)))))
    (doseq [{:keys [^File file name]} chapters]
      (add! (.getBytes (str "\u0000" name "\u0000") "UTF-8"))
      (add! (java.nio.file.Files/readAllBytes (.toPath file))))
    (add! (.getBytes (pr-str (sort (map str/lower-case skip))) "UTF-8"))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(defn resource
  "The bytes and media type of image `name` the book at `path` shows, nil
  unless it is an image inside its repository (or folder): {:bytes :type}."
  [path name]
  (let [dir  (let [f (.getCanonicalFile (io/file (str path)))] (if (.isDirectory f) f (.getParentFile f)))
        base (or (:root (repository dir)) dir)
        f    (.getCanonicalFile (io/file base (str name)))
        type (book/media-type (.getName f))]
    (when (and (.isFile f) type (str/starts-with? type "image/")
               (.startsWith (.toPath f) (.toPath base)))
      {:bytes (java.nio.file.Files/readAllBytes (.toPath f)) :type type})))
