(ns robertluo.fly-learner.ingest.epub
  "EPUB source: the book's documents (XHTML, in spine order) cut into pages,
  so that it is read and pointed at as a PDF is: by page.

  An EPUB has no pages of its own. Its text is cut, between block elements
  (paragraphs, code, lists, tables, figures, headings), into pages of about
  `page-chars` characters, a document (chapter file) starting a new one.
  `extract` gives the pages' text as blocks for the knowledge base, the
  same shape robertluo.fly-learner.ingest.pdf gives:

    {:kind :heading :depth d :title \"1.1 The Elements of Programming\" :page p}
    {:kind :text    :text \"...\" :page p}

  and `page-html` the same pages as HTML to show, so a block's page is where
  the reader sees it. Both read the documents cleaned the same way (`clean`):
  no scripts, styles or navigation, an SVG in an <object> as an <img>.

  Headings take their depth from the book's table of contents (nav, else
  NCX); one it does not list is that much deeper than the listed heading
  before it, and one above the first listed (a title page) is text. Code
  and display formulas join the paragraph before them on the page.
  Footnotes are shown where the book puts them, but their text goes after
  the paragraph citing them: it is about that paragraph."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.util.zip ZipFile)
           (org.jsoup Jsoup)
           (org.jsoup.nodes Comment Document Document$OutputSettings$Syntax Element Node TextNode)
           (org.jsoup.parser Parser)))

;;=============================================================================
;; The package: metadata, spine, table of contents
;;=============================================================================

(defn- entry-text [^ZipFile zip name]
  (when-let [e (.getEntry zip name)]
    (with-open [in (.getInputStream zip e)]
      (slurp in :encoding "UTF-8"))))

(defn- xml [s] (Jsoup/parse ^String s "" (Parser/xmlParser)))

(defn- resolve-path
  "Path `href` (URL-encoded, maybe with a #fragment) relative to the zip
  entry `base` -> [entry-path fragment]."
  [base href]
  (let [[path frag] (str/split href #"#" 2)
        dir  (let [i (str/last-index-of base "/")] (if i (subs base 0 (inc i)) ""))
        full (if (str/blank? path) base (str dir path))
        norm (.getPath (.normalize (URI. nil nil (str "/" (java.net.URLDecoder/decode full "UTF-8")) nil)))]
    [(subs norm 1) (not-empty frag)]))

^:rct/test
(comment
  (resolve-path "html/1_002e1.xhtml" "1_002e2.xhtml#g_t1_002e2") ;=> ["html/1_002e2.xhtml" "g_t1_002e2"]
  (resolve-path "html/1_002e1.xhtml" "#FOOT4") ;=> ["html/1_002e1.xhtml" "FOOT4"]
  (resolve-path "OEBPS/text/c1.xhtml" "../img/a%20b.svg") ;=> ["OEBPS/img/a b.svg" nil]
  )

(defn- toc-depths
  "{[entry fragment] depth} from the nav document's toc (an <ol> per level)
  or an NCX's navPoints."
  [^ZipFile zip nav-path ncx-path]
  (if-let [nav (some->> nav-path (entry-text zip) xml)]
    (let [toc (or (first (filter #(re-find #"toc" (.attr ^Element % "epub:type")) (.select nav "nav")))
                  (first (.select nav "nav")))]
      (into {} (for [^Element a (some-> toc (.select "a[href]"))]
                 [(resolve-path nav-path (.attr a "href"))
                  (count (filter #(= "ol" (.tagName ^Element %)) (.parents a)))])))
    (when-let [ncx (some->> ncx-path (entry-text zip) xml)]
      (into {} (for [^Element c (.select ncx "navPoint > content[src]")]
                 [(resolve-path ncx-path (.attr c "src"))
                  (count (filter #(= "navPoint" (.tagName ^Element %)) (.parents c)))])))))

(defn- package
  "{:title :docs [entry-path ..] (spine order, linear ones) :toc {[entry frag] depth}}."
  [^ZipFile zip]
  (let [opf-path (some-> (entry-text zip "META-INF/container.xml") xml
                         (.selectFirst "rootfile") (.attr "full-path"))
        opf      (xml (or (entry-text zip opf-path) (throw (ex-info "Not an EPUB: no package document" {}))))
        items    (into {} (for [^Element i (.select opf "manifest > item")]
                            [(.attr i "id") {:path (first (resolve-path opf-path (.attr i "href")))
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
  "Leaves of each document ([[entry leaves] ..]) with :page and :doc: a new
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
  "The book at `path`: {:title :pages n :leaves [..] :places {[entry id] page}
  :zip-paths} (documents parsed and cleaned)."
  [path]
  (with-open [zip (ZipFile. (io/file path))]
    (let [{:keys [title docs toc]} (package zip)
          parsed (vec (for [d docs :let [s (entry-text zip d)] :when s]
                        [d (leaves (clean (doto (Jsoup/parse ^String s "" (Parser/xmlParser))
                                            (-> .outputSettings (.syntax Document$OutputSettings$Syntax/html)
                                                (.prettyPrint false)))))]))
          ls     (paginate parsed)]
      {:title  (or title (.getName (io/file path)))
       :toc    toc
       :pages  (or (:page (peek ls)) 0)
       :leaves ls
       :places (reduce (fn [m {:keys [doc ids page]}]
                         (reduce #(if (contains? %1 [doc %2]) %1 (assoc %1 [doc %2] page))
                                 (if (contains? m [doc nil]) m (assoc m [doc nil] page))
                                 ids))
                       {} ls)})))

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

(defn- joins?
  "Code or a display formula: part of the paragraph before it."
  [{:keys [nodes]}]
  (let [[^Node n] nodes]
    (and (= 1 (count nodes)) (instance? Element n)
         (or (= "pre" (.tagName ^Element n))
             (= "math" (.tagName ^Element n))
             (and (#{"div" "p"} (.tagName ^Element n))
                  (let [kids (.children ^Element n)]
                    (and (seq kids) (every? #(#{"pre" "math"} (.tagName ^Element %)) kids)
                         (str/blank? (.ownText ^Element n)))))))))

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
  "[entry fragment] each link in `leaf`'s nodes points at (internal links only)."
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
                     (conj out {:kind :heading :depth depth :title (heading-title (first nodes)) :page page})

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
  "Return {:title .. :pages n :blocks [...]} (as robertluo.fly-learner.ingest.pdf/extract)."
  [path]
  (let [book (read-book path)]
    {:title (:title book) :pages (:pages book) :blocks (blocks book)}))

;;=============================================================================
;; HTML: the pages to show
;;=============================================================================

(defn- show!
  "Make `leaf`'s nodes fit to show inside the app's page: resources by
  `resource-url` (entry path -> URL), a link inside the book as the page it
  goes to (data-goto), one outside opening apart; no ids (they would clash
  with the app's), classes prefixed \"b-\" (the book's own, not the app's)."
  [{:keys [nodes doc]} places resource-url]
  (doseq [^Node n nodes :when (instance? Element n)
          ^Element e (.select ^Element n "*")]
    (case (.tagName e)
      "img"   (when (seq (.attr e "src"))
                (.attr e "src" ^String (resource-url (first (resolve-path doc (.attr e "src"))))))
      "image" (doseq [k ["href" "xlink:href"] :when (seq (.attr e ^String k))]
                (.attr e ^String k ^String (resource-url (first (resolve-path doc (.attr e ^String k))))))
      "a"     (when (.hasAttr e "href")
                (let [href (.attr e "href")]
                  (if (re-find #"^(?i)(https?|mailto):" href)
                    (doto e (.attr "target" "_blank") (.attr "rel" "noopener"))
                    (let [[path frag] (resolve-path doc href)]
                      (if-let [p (or (places [path frag]) (places [path nil]))]
                        (doto e (.attr "href" (str "#page-" p)) (.attr "data-goto" (str p)))
                        (.removeAttr e "href"))))))
      nil)
    (.removeAttr e "id")
    (when (seq (.className e))
      (.attr e "class" ^String (str/join " " (map #(str "b-" %) (str/split (str/trim (.className e)) #"\s+")))))))

(defn page-html
  "The book at `path` as pages of HTML to show: [html ..], page p at p - 1.
  `resource-url`: an entry path (an image) -> the URL it is served at."
  [path resource-url]
  (let [{:keys [pages leaves places]} (read-book path)
        by-page (group-by :page leaves)]
    (vec (for [p (range 1 (inc pages))]
           (str/join "\n"
                     (for [{:keys [nodes footnote] :as l} (by-page p)]
                       (do (show! l places resource-url)
                           (let [html (str/join (map #(.outerHtml ^Node %) nodes))]
                             (if footnote (str "<div class=\"note\">" html "</div>") html)))))))))

(defn resource
  "The bytes and media type of entry `entry` of the book at `path`, nil when
  there is none: {:bytes :type}."
  [path entry]
  (with-open [zip (ZipFile. (io/file path))]
    (when-let [e (.getEntry zip ^String entry)]
      (when-not (.isDirectory e)
        {:bytes (with-open [in (.getInputStream zip e)] (.readAllBytes in))
         :type  (or (some (fn [[re t]] (when (re-find re (str/lower-case entry)) t))
                          [[#"\.svg$" "image/svg+xml"] [#"\.png$" "image/png"] [#"\.jpe?g$" "image/jpeg"]
                           [#"\.gif$" "image/gif"] [#"\.webp$" "image/webp"] [#"\.css$" "text/css"]
                           [#"\.(ttf|otf|woff2?)$" "font/woff"]])
                    "application/octet-stream")}))))
