(ns robertluo.fly-learner.ingest.book
  "A book of HTML documents, read as pages: what an EPUB
  (robertluo.fly-learner.ingest.epub) and a folder of Markdown files
  (robertluo.fly-learner.ingest.markdown) both are once opened. A format
  gives a package:

    {:title     \"A Small Book\"
     :docs      [[name document] ..]   ; reading order; a name is a path (\"OEBPS/c1.xhtml\")
     :toc       {[name fragment] depth} ; the headings its table of contents lists
     :redirects {name name}             ; (optional) old names links may still use
     :outside   (fn [name fragment])}   ; (optional) the URL of a link to what the book does not hold

  The documents' text is cut, between block elements (paragraphs, code,
  lists, tables, figures, headings), into pages of about `page-chars`
  characters, a document starting a new one. `extract` gives the pages'
  text as blocks for the knowledge base, the same shape
  robertluo.fly-learner.ingest.pdf gives:

    {:kind :heading :depth d :title \"1.1 The Elements of Programming\" :page p}
    {:kind :text    :text \"...\" :page p}

  and `page-html` the same pages as HTML to show, so a block's page is where
  the reader sees it. Both read the documents cleaned the same way (`clean`):
  no scripts, styles or navigation, an SVG in an <object> as an <img>.

  Headings take their depth from the table of contents; one it does not
  list is that much deeper than the listed heading before it, and one above
  the first listed (a title page) is text; a heading's data-keywords
  (comma-separated) are its section's. Code and display formulas (<math>,
  an element with data-tex=\"display\") join the paragraph before them on
  the page. A block with data-aside is shown but not read (a chapter's
  tags, its byline). Footnotes are shown where the book puts them, but their
  text goes after the paragraph citing them: it is about that paragraph."
  (:require [clojure.string :as str])
  (:import (java.net URI)
           (org.jsoup.nodes Comment Document Document$OutputSettings$Syntax Element Node TextNode)))

(defn resolve-path
  "Path `href` (URL-encoded, maybe with a #fragment) relative to the document
  named `base` -> [name fragment]. A path starting with / is from the top
  (of the zip, of the repository)."
  [base href]
  (let [[path frag] (str/split href #"#" 2)
        dir  (let [i (str/last-index-of base "/")] (if i (subs base 0 (inc i)) ""))
        full (cond (str/blank? path)            base
                   (str/starts-with? path "/")  (subs path 1)
                   :else                        (str dir path))
        norm (.getPath (.normalize (URI. nil nil (str "/" (java.net.URLDecoder/decode full "UTF-8")) nil)))]
    [(subs norm 1) (not-empty frag)]))

^:rct/test
(comment
  (resolve-path "html/1_002e1.xhtml" "1_002e2.xhtml#g_t1_002e2") ;=> ["html/1_002e2.xhtml" "g_t1_002e2"]
  (resolve-path "html/1_002e1.xhtml" "#FOOT4") ;=> ["html/1_002e1.xhtml" "FOOT4"]
  (resolve-path "OEBPS/text/c1.xhtml" "../img/a%20b.svg") ;=> ["OEBPS/img/a b.svg" nil]
  (resolve-path "pkg/docs/sdk.md" "../examples/a.ts") ;=> ["pkg/examples/a.ts" nil]
  (resolve-path "pkg/docs/sdk.md" "/README.md") ;=> ["README.md" nil]
  )

(defn media-type
  "The media type of a file named `name`, by its extension (images, styles,
  fonts); nil when none of these."
  [name]
  (some (fn [[re t]] (when (re-find re (str/lower-case name)) t))
        [[#"\.svg$" "image/svg+xml"] [#"\.png$" "image/png"] [#"\.jpe?g$" "image/jpeg"]
         [#"\.gif$" "image/gif"] [#"\.webp$" "image/webp"] [#"\.avif$" "image/avif"] [#"\.css$" "text/css"]
         [#"\.(ttf|otf|woff2?)$" "font/woff"]]))

;;=============================================================================
;; Cleaning a document
;;=============================================================================

(def ^:private dropped
  "What is never shown nor read: scripts, styles, forms, embedded pages, navigation."
  "script, style, link, meta, iframe, frame, embed, form, input, button, select, textarea, nav, noscript")

(defn- clean
  "Document `doc` without `dropped` elements, comments, event handlers and
  script links; an SVG <object> becomes an <img> (an image runs no script)."
  [^Document doc]
  (.remove (.select doc ^String dropped))
  (doseq [^Element o (.select doc "object[data]")]
    (let [img (doto (Element. "img")
                (.attr "src" (.attr o "data"))
                (.attr "alt" ""))]
      (when (seq (.attr o "style")) (.attr img "style" (.attr o "style")))
      (.replaceWith o img)))
  (.remove (.select doc "object"))
  (doseq [^Element e (.select doc "*")]
    (doseq [a (vec (.attributes e))
            :let [k (str/lower-case (.getKey ^org.jsoup.nodes.Attribute a))
                  v (str/lower-case (str/trim (.getValue ^org.jsoup.nodes.Attribute a)))]
            :when (or (str/starts-with? k "on") (re-find #"^(java|vb)script:" v))]
      (.removeAttr e (.getKey ^org.jsoup.nodes.Attribute a))))
  (letfn [(strip [^Node n]
            (doseq [^Node c (vec (.childNodes n))]
              (if (instance? Comment c) (.remove c) (strip c))))]
    (strip doc))
  doc)

;;=============================================================================
;; Blocks: the document as leaves to cut into pages
;;=============================================================================

(def ^:private leaf-tags
  #{"p" "pre" "h1" "h2" "h3" "h4" "h5" "h6" "ul" "ol" "dl" "table" "blockquote" "figure" "hr" "address" "math" "svg"})

(def ^:private container-tags #{"body" "section" "article" "main" "div" "header" "footer" "aside"})

(defn- heading-level
  "1-6 when node `n` is a heading element, else nil."
  [n]
  (when (instance? Element n) (some-> (re-matches #"h([1-6])" (.tagName ^Element n)) second parse-long)))

(defn- footnote?
  "Is `e` a footnote or the book's footnotes?"
  [^Element e]
  (or (re-find #"(?i)(?:^|\s)(?:foot|end|rear)notes?(?:\s|$)" (.className e))
      (re-find #"(?:foot|end|rear)notes?" (.attr e "epub:type"))
      (re-find #"doc-(?:foot|end)notes?" (.attr e "role"))))

(defn- container? [^Element e]
  (and (container-tags (.tagName e))
       (some #(or (leaf-tags (.tagName ^Element %)) (container-tags (.tagName ^Element %))) (.children e))))

(defn- node-text [^Node n]
  (cond (instance? TextNode n) (.text ^TextNode n)
        (instance? Element n)  (.text ^Element n)
        :else ""))

(defn- ids [nodes]
  (for [^Node n nodes :when (instance? Element n)
        ^Element e (.select ^Element n "[id]")]
    (.id e)))

(defn- leaves
  "The document's body as leaves in reading order: [{:nodes [node ..] :ids
  [..] :footnote unit-or-nil}], a leaf being a block element or a run of
  inline content between blocks. :ids are the ids it holds or that point
  just before it (anchors, its containers); :footnote the id of the footnote
  it is part of (\"\" when in the footnotes but no single one)."
  [^Document doc]
  (let [out (volatile! []) pending (volatile! []) run (volatile! [])
        flush! (fn [note]
                 (let [nodes @run]
                   (vreset! run [])
                   (when (seq nodes)
                     (if (or (re-find #"[\p{L}\p{N}]" (apply str (map node-text nodes)))
                             (some #(and (instance? Element %) (seq (.select ^Element % "img, math, svg"))) nodes))
                       (do (vswap! out conj {:nodes nodes :ids (into @pending (ids nodes)) :footnote note})
                           (vreset! pending []))
                       (vswap! pending into (ids nodes))))))]
    (letfn [(walk [^Element el note]
              (let [note (cond (and note (seq (.id el))) (.id el)
                               (footnote? el)            (or note "")
                               :else                      note)]
                (when (seq (.id el)) (vswap! pending conj (.id el)))
                (doseq [^Node c (.childNodes el)]
                  (cond
                    (and (instance? Element c) (container? c))
                    (do (flush! note) (walk c note))

                    (and (instance? Element c) (or (leaf-tags (.tagName ^Element c))
                                                   (container-tags (.tagName ^Element c))))
                    (do (flush! note)
                        (vswap! run conj c)
                        (flush! (if (and note (seq (.id ^Element c))) (.id ^Element c) note)))

                    (and (instance? TextNode c) (str/blank? (.text ^TextNode c)) (empty? @run))
                    nil

                    :else (vswap! run conj c)))
                (flush! note)))]
      (when-let [body (.body doc)] (walk body nil)))
    @out))

;;=============================================================================
;; Pages
;;=============================================================================

(def page-chars
  "About how much text a page holds: a printed page's worth."
  3000)

(defn- leaf-text [{:keys [nodes]}]
  (str/trim (str/replace (apply str (map node-text nodes)) #"\s+" " ")))

(defn- paginate
  "Leaves of each document ([[name leaves] ..]) with :page and :doc: a new
  page for each document, else once a page holds `page-chars` (or half of
  that before a heading)."
  [docs]
  (loop [[[doc ls] & more] docs, page 0, out []]
    (if-not doc
      out
      (let [[page ls]
            (reduce (fn [[page acc chars] l]
                      (let [n     (count (leaf-text l))
                            head? (heading-level (first (:nodes l)))
                            new?  (or (nil? chars) (>= chars page-chars) (and head? (>= chars (/ page-chars 2))))
                            page  (if new? (inc page) page)]
                        [page (conj acc (assoc l :page page :doc doc)) (+ n (if new? 0 chars))]))
                    [page [] nil]
                    ls)]
        (recur more page (into out ls))))))

(defn- read-book
  "The book of package `pkg` (see the ns doc): {:title :toc :pages n :leaves
  [..] :places {[name id] page} :outside} (documents cleaned)."
  [{:keys [title docs toc redirects outside]}]
  (let [parsed (vec (for [[name ^Document doc] docs]
                      [name (leaves (clean (doto doc
                                             (-> .outputSettings (.syntax Document$OutputSettings$Syntax/html)
                                                 (.prettyPrint false)))))]))
        ls     (paginate parsed)
        places (reduce (fn [m {:keys [doc ids page]}]
                         (reduce #(if (contains? %1 [doc %2]) %1 (assoc %1 [doc %2] page))
                                 (if (contains? m [doc nil]) m (assoc m [doc nil] page))
                                 ids))
                       {} ls)]
    {:title   title
     :toc     toc
     :pages   (or (:page (peek ls)) 0)
     :leaves  ls
     :outside outside
     :places  (reduce-kv (fn [m from to] (if-let [p (m [to nil])] (assoc m [from nil] p) m))
                         places redirects)}))

;;=============================================================================
;; Text: blocks for the knowledge base
;;=============================================================================

(defn- heading-title
  "A heading's text, its parts spaced (\"<span>1.1</span><span>The..</span>\")."
  [^Element h]
  (str/trim (str/replace (str/join " " (map node-text (.childNodes h))) #"\s+" " ")))

(defn- block-text [{:keys [nodes] :as l}]
  (let [[^Node n] nodes]
    (if (and (= 1 (count nodes)) (instance? Element n) (= "pre" (.tagName ^Element n)))
      (str/trim (str/replace (.wholeText ^Element n) #"\r" ""))
      (leaf-text l))))

(defn- display?
  "Is element `e` code or a display formula?"
  [^Element e]
  (or (#{"pre" "math"} (.tagName e)) (= "display" (.attr e "data-tex"))))

(defn- joins?
  "Code or a display formula: part of the paragraph before it."
  [{:keys [nodes]}]
  (let [[^Node n] nodes]
    (and (= 1 (count nodes)) (instance? Element n)
         (or (display? n)
             (and (#{"div" "p"} (.tagName ^Element n))
                  (let [kids (.children ^Element n)]
                    (and (seq kids) (every? display? kids)
                         (str/blank? (.ownText ^Element n)))))))))

(defn- aside?
  "Shown, not read: a block marked data-aside."
  [{:keys [nodes]}]
  (let [[^Node n] nodes]
    (and (instance? Element n) (.hasAttr ^Element n "data-aside"))))

(defn- keywords [^Element h]
  (not-empty (vec (remove str/blank? (map str/trim (str/split (.attr h "data-keywords") #","))))))

(defn- heading-depths
  "Depth of each leaf that is a heading outside footnotes (nil: not a
  heading), from the table of contents `toc`; without one, by rank of the
  heading levels used."
  [toc ls]
  (let [levels (vec (sort (distinct (keep #(when-not (:footnote %) (some-> (first (:nodes %)) heading-level)) ls))))]
    (if (empty? toc)
      (mapv #(when-let [lv (and (not (:footnote %)) (heading-level (first (:nodes %))))]
               (inc (.indexOf levels lv)))
            ls)
      (first
       (reduce (fn [[out last-depth last-level first-of] {:keys [doc ids footnote nodes]}]
                 (let [lv (when-not footnote (heading-level (first nodes)))
                       listed (when lv (or (some #(toc [doc %]) ids)
                                           (when-not (first-of doc) (toc [doc nil]))))
                       depth  (cond listed listed
                                    (and lv last-level (> lv last-level)) (+ last-depth (- lv last-level)))]
                   [(conj out depth)
                    (if listed listed last-depth)
                    (if listed lv last-level)
                    (if lv (conj first-of doc) first-of)]))
               [[] nil nil #{}]
               ls)))))

(defn- links
  "[name fragment] each link in `leaf`'s nodes points at (internal links only)."
  [{:keys [nodes doc]}]
  (for [^Node n nodes :when (instance? Element n)
        ^Element a (.select ^Element n "a[href]")
        :let [href (.attr a "href")]
        :when (not (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*:" href))]
    (resolve-path doc href)))

(defn- with-footnotes
  "Blocks with each footnote (the run of blocks of one :note) moved after
  the first block citing it; footnotes no block cites stay where they are."
  [bs]
  (let [notes  (group-by :note (filter :note bs))
        cites  (fn [b] (distinct (filter notes (:cites b))))
        placed (set (mapcat cites (remove :note bs)))]
    (vec (mapcat (fn [b]
                   (cond (:note b)  (when-not (placed (:note b)) [b])
                         :else      (cons b (mapcat notes (cites b)))))
                 ;; a footnote cited twice: after the first citing block only
                 (let [seen (volatile! #{})]
                   (map (fn [b] (if (:note b) b (let [c (remove @seen (cites b))]
                                                  (vswap! seen into c)
                                                  (assoc b :cites c))))
                        bs))))))

(defn- blocks
  "The book's leaves as blocks (see the ns doc), in reading order."
  [{:keys [toc leaves]}]
  (->> (map vector leaves (heading-depths toc leaves))
       (reduce (fn [out [{:keys [nodes footnote page doc] :as l} depth]]
                 (let [prev (peek out)
                       note (when (seq footnote) [doc footnote])]
                   (cond
                     depth
                     (conj out (cond-> {:kind :heading :depth depth :title (heading-title (first nodes)) :page page}
                                 (keywords (first nodes)) (assoc :keywords (keywords (first nodes)))))

                     (aside? l)
                     out

                     ;; a footnote's heading ("Footnotes", its number) is no section
                     (and footnote (heading-level (first nodes)))
                     out

                     (and (joins? l) (= :text (:kind prev)) (= page (:page prev)) (= note (:note prev)))
                     (conj (pop out) (-> prev
                                         (update :text str "\n" (block-text l))
                                         (update :cites into (links l))))

                     :else
                     (let [t (block-text l)]
                       (cond-> out
                         (seq t) (conj (cond-> {:kind :text :text t :page page :cites (vec (links l))}
                                         note (assoc :note note))))))))
               [])
       with-footnotes
       (mapv #(dissoc % :cites :note))))

(defn extract
  "Return {:title .. :pages n :blocks [...]} (as robertluo.fly-learner.ingest.pdf/extract)
  for package `pkg`."
  [pkg]
  (let [book (read-book pkg)]
    {:title (:title book) :pages (:pages book) :blocks (blocks book)}))

;;=============================================================================
;; HTML: the pages to show
;;=============================================================================

(defn- external? [href] (re-find #"^(?i)(https?|mailto|data):" href))

(defn- show!
  "Make `leaf`'s nodes fit to show inside the app's page: resources by
  `resource-url` (name -> URL; one on the web as it is), a link inside the
  book as the page it goes to (data-goto), one outside it opening apart (a
  name the book does not hold by `outside`, else no link); no ids (they
  would clash with the app's), classes prefixed \"b-\" (the book's own, not
  the app's)."
  [{:keys [nodes doc]} places outside resource-url]
  (doseq [^Node n nodes :when (instance? Element n)
          ^Element e (.select ^Element n "*")]
    (case (.tagName e)
      "img"   (when (and (seq (.attr e "src")) (not (external? (.attr e "src"))))
                (.attr e "src" ^String (resource-url (first (resolve-path doc (.attr e "src"))))))
      "image" (doseq [k ["href" "xlink:href"] :when (seq (.attr e ^String k))]
                (.attr e ^String k ^String (resource-url (first (resolve-path doc (.attr e ^String k))))))
      "a"     (when (.hasAttr e "href")
                (let [href (.attr e "href")]
                  (if (external? href)
                    (doto e (.attr "target" "_blank") (.attr "rel" "noopener"))
                    (let [[path frag] (resolve-path doc href)]
                      (if-let [p (or (places [path frag]) (places [path nil]))]
                        (doto e (.attr "href" (str "#page-" p)) (.attr "data-goto" (str p)))
                        (if-let [url (when outside (outside path frag))]
                          (doto e (.attr "href" ^String url) (.attr "target" "_blank") (.attr "rel" "noopener"))
                          (.removeAttr e "href")))))))
      nil)
    (.removeAttr e "id")
    (when (seq (.className e))
      (.attr e "class" ^String (str/join " " (map #(str "b-" %) (str/split (str/trim (.className e)) #"\s+")))))))

(defn page-html
  "The book of package `pkg` as pages of HTML to show: [html ..], page p at
  p - 1. `resource-url`: a name (an image) -> the URL it is served at."
  [pkg resource-url]
  (let [{:keys [pages leaves places outside]} (read-book pkg)
        by-page (group-by :page leaves)]
    (vec (for [p (range 1 (inc pages))]
           (str/join "\n"
                     (for [{:keys [nodes footnote] :as l} (by-page p)]
                       (do (show! l places outside resource-url)
                           (let [html (str/join (map #(.outerHtml ^Node %) nodes))]
                             (if footnote (str "<div class=\"note\">" html "</div>") html)))))))))
