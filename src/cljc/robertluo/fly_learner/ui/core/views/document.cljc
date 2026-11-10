(ns robertluo.fly-learner.ui.core.views.document
  "The source document beside the reading: every page in one scrolling
  column, the node's text highlighted, its key sentences and the learner's
  quotes marked, bookmarks and the page box. All of it is about pages; only
  drawing a page depends on the source's kind (`source-doc`): a PDF's by
  pdf.js (a canvas, its text layer over it: robertluo.fly-learner.ui.pdf),
  an EPUB's or a Markdown folder's as HTML the server cut into pages
  (robertluo.fly-learner.ui.html).
  Finding texts on a page and scrolling are robertluo.fly-learner.ui.pages.
  The DOM hooks are inert on the JVM."
  (:require [robertluo.fly-learner.ui.core.api :as api]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.highlight :as hl]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.panes :as panes]
            [robertluo.fly-learner.ui.core.views.template :as t]
            #?(:cljs [robertluo.fly-learner.ui.html :as html])
            #?(:cljs [robertluo.fly-learner.ui.pages :as pages])
            #?(:cljs [robertluo.fly-learner.ui.pdf :as pdf])
            #?(:cljs [robertluo.fly-learner.ui.select :as select])
            #?(:cljs [robertluo.fly-learner.ui.zoom :as zoom])))

(defn source-doc
  "How the pages of `source` ({:source/id :source/type}) are drawn:
  {:kind :pdf|:html :url ..}, the url also naming the document in the state."
  [{:source/keys [id type]}]
  (if (#{:epub :markdown} type)
    {:kind :html :url (api/source-pages-url id)}
    {:kind :pdf :url (api/source-url id)}))

^:rct/test
(comment
  (= {:kind :html :url (api/source-pages-url "s")} (source-doc {:source/id "s" :source/type :epub})) ;=> true
  (= {:kind :html :url (api/source-pages-url "s")} (source-doc {:source/id "s" :source/type :markdown})) ;=> true
  (= {:kind :pdf :url (api/source-url "s")} (source-doc {:source/id "s" :source/type :pdf})) ;=> true
  (= {:kind :pdf :url (api/source-url "s")} (source-doc {:source/id "s"})) ;=> true
  )

(defn- pdf-canvas-attrs
  "Canvas attrs that (re)draw page `p` of `url` via pdf.js after each render.
  On the JVM (tests, no DOM) the canvas is inert."
  #?(:cljs [url p on-pages] :clj [_url _p _on-pages])
  #?(:cljs (t/on-render #(pdf/render-page! % url p on-pages))
     :clj  {}))

(defn- pages-attrs
  "Attrs of the column of pages: kept at the page shown, and at the node's
  highlight while following the node (`sync`, see pages/sync-scroll!; an
  attribute too, so the hook runs when it changes); scrolling it turns the
  page, one step in history once it pauses. Loads the pages when they are
  to be (`load-pages`, HTML ones); finds the highlight `key` once they are
  there (`ready?`). Inert on the JVM."
  #?(:cljs [doc load-pages ready? following? sync key texts found dispatch!]
     :clj  [_doc _load-pages _ready? following? sync _key _texts _found _dispatch!])
  (merge {:data-following (str (boolean following?)) :data-sync (pr-str sync)}
         #?(:cljs (t/on-render
                   (fn [pane]
                       (when load-pages (load-pages))
                       (zoom/pinch! pane (fn [f] (dispatch! {:db #(db/doc-zoomed % (* f (db/doc-zoom %)))})))
                       (pages/watch-scroll! pane
                                          (fn [p] (dispatch! {:db #(db/doc-scrolled % p)}))
                                          ;; leaving the node's page is one step back in history;
                                          ;; the left pane then follows to the node on that page
                                          (fn [was-following?]
                                            (dispatch! {:history (if was-following? :push :replace)
                                                        :pull :page-sync})))
                       (when (and ready? (not found))
                         (pages/ensure-highlight! doc key pane texts
                                                  (fn [k result]
                                                    (dispatch! {:db #(db/highlight-found % k result)}))))
                       (pages/sync-scroll! pane sync)))
            :clj  {})))

(defn- page-anchor
  "Attrs that make a page's text selectable to react to (robertluo.fly-learner.ui.select)."
  [source-id p]
  {:data-anchor "" :data-field "page" :data-source (str source-id) :data-page (str p)})

(defn- text-layer-attrs
  "The PDF page's text, selectable to react to (pdf.js TextLayer, filled
  after render); says when it is laid out. Inert on the JVM."
  #?(:cljs [url p source-id dispatch!] :clj [_url p source-id _dispatch!])
  (merge (page-anchor source-id p)
         #?(:cljs (t/on-render #(pdf/render-text-layer! % url p
                                                        (fn [] (dispatch! {:db (fn [d] (db/text-layer-ready d url p))}))))
            :clj  {})))

(defn- quote-marks-attrs
  "Marks layer attrs that find the quotes keyed `k` in the page's text
  (its sibling, a text layer or the page itself) once that is laid out.
  Inert on the JVM."
  #?(:cljs [k quotes ready? placed? dispatch!] :clj [k _quotes ready? _placed? _dispatch!])
  (merge {:data-quotes (pr-str [k ready?])}
         #?(:cljs (t/on-render
                   (fn [^js node]
                       (when (and ready? (not placed?))
                         (when-let [layer (some-> (.-parentElement node) (.querySelector "[data-anchor]"))]
                           (let [placed (into {} (for [{:keys [idea anchor]} quotes
                                                       :let [rects (select/quote-rects layer anchor)]
                                                       :when rects]
                                                   [(:idea/id idea) rects]))]
                             ;; not inside this render
                             (js/setTimeout #(dispatch! {:db (fn [d] (db/doc-quotes-placed d k placed))}) 0))))))
            :clj  {})))

(defn- doc-quote-marks
  "Ideas on text selected on this page, marked where that text is."
  [db sid url p dispatch!]
  (let [quotes (db/doc-quotes db sid p)
        k      (db/doc-quotes-key sid p quotes)
        placed (get-in db [:doc :placed k])
        by-id  (into {} (map (juxt (comp :idea/id :idea) :idea)) quotes)]
    [:div.doc-idea-marks (quote-marks-attrs k quotes (db/text-layer-ready? db url p)
                                            (db/doc-quotes-placed? db k) dispatch!)
     (for [[id rects] placed
           :let [{:idea/keys [stance]} (by-id id)]
           :when stance
           [i {:keys [x y w h]}] (map-indexed vector rects)]
       [:div.idea-mark {:replicant/key (str id "-" i) :class (name stance)
                        :style {:left (t/pct x) :top (t/pct y) :width (t/pct w) :height (t/pct h)}}])]))

(defn- key-marks-attrs
  "Attrs that find the key sentences `kps` on page `p` once, unless placed.
  Inert on the JVM."
  #?(:cljs [doc p kps placed? dispatch!] :clj [_doc p kps _placed? _dispatch!])
  (merge {:data-keys (pr-str [p (count kps)])}
         #?(:cljs (t/on-render
                   (fn [node]
                       (when (and (seq kps) (not placed?))
                         (pages/find-each! doc p node (mapv :quote kps)
                                           (fn [placed] (dispatch! {:db #(db/keypoints-placed % (:url doc) p kps placed)}))))))
            :clj  {})))

(defn- doc-key-marks
  "The key sentences on this page: tinted, their label in the page's margin."
  [db sid doc p dispatch!]
  (let [kps    (db/page-keypoints db sid p)
        {:keys [found left aspect] :as placed} (db/keypoint-rects db (:url doc) p kps)
        margin (max 0.03 (- (or left 0.1) 0.012))
        found  (sort-by (comp :y first second) found)
        ;; labels close together stack rather than overlap
        tops   (hl/stack-labels (for [[{:keys [label]} rects] found] {:y (:y (first rects)) :label label})
                                (- margin 0.008) (or aspect (db/doc-aspect db sid) 0.75))]
    [:div.doc-key-marks (key-marks-attrs doc p kps (some? placed) dispatch!)
     (for [[i [{:keys [label quote]} rects] top] (map vector (range) found tops)]
       [:div {:replicant/key (str i quote)}
        [:div.key-label {:title label :style {:top (t/pct top) :width (t/pct (- margin 0.008))}} label]
        (for [[j {:keys [x y w h]}] (map-indexed vector rects)]
          [:div.key-mark {:replicant/key j :style {:left (t/pct x) :top (t/pct y) :width (t/pct w) :height (t/pct h)}}])])]))

(defn- open-bar-attrs
  "Attrs that find node `id`'s `texts` on page `p` once. Inert on the JVM."
  #?(:cljs [doc p id texts pending? dispatch!] :clj [_doc p _id texts _pending? _dispatch!])
  (merge {:data-open (pr-str [p (count texts)])}
         #?(:cljs (t/on-render
                   (fn [node]
                       (when (and pending? (seq texts))
                         (pages/find-each! doc p node texts
                                           (fn [{:keys [rects]}]
                                             (dispatch! {:db #(db/open-placed % (:url doc) p id texts rects)}))))))
            :clj  {})))

(defn- open-bar
  "What is open on the left, where it is on this page: a bar in the margin
  along it, on every page it covers."
  [db doc {:node/keys [id] :as node} q dispatch!]
  (let [texts (db/open-texts node q)
        span  (when texts (db/open-span db (:url doc) q id texts))]
    [:div.doc-marks (open-bar-attrs doc q id texts (= :pending span) dispatch!)
     (when (vector? span)
       (let [[top bottom left] span]
         [:div.open-bar {:title "Open on the left"
                         :style {:left (t/pct (max 0.004 (- left 0.022))) :top (t/pct top)
                                 :height (t/pct (- bottom top))}}]))]))

(defn- marks
  "What goes over a page, whatever draws it: key sentences, the learner's
  quotes, the bar along what is open on the left."
  [db {:keys [sid doc node]} dispatch! q]
  (list
   (doc-key-marks db sid doc q dispatch!)
   (doc-quote-marks db sid (:url doc) q dispatch!)
   (open-bar db doc node q dispatch!)))

(defn- html-page-attrs
  "Page `p`'s HTML, its text selectable; says when it is laid out (images
  in) if `watch?` (near the page shown), and a link to another page of the
  book goes there. Inert on the JVM."
  #?(:cljs [url p html source-id watch? dispatch!] :clj [_url p html source-id _watch? _dispatch!])
  (merge (page-anchor source-id p)
         {:innerHTML html}
         #?(:cljs {:replicant/on-render
                   (fn [{:replicant/keys [node]}]
                     (when (and watch? html)
                       (html/when-laid-out! node #(dispatch! {:db (fn [d] (db/text-layer-ready d url p))}))))
                   :on {:click (fn [^js e]
                                 (when-let [a (some-> (.-target e) (.closest "[data-goto]"))]
                                   (.preventDefault e)
                                   (dispatch! {:db #(db/doc-goto % source-id (js/parseInt (.. a -dataset -goto) 10))
                                               :history :push})))}}
            :clj  {})))

(defn- pdf-layers
  "A PDF page drawn by pdf.js: its canvas and its text layer, the marks over them."
  [db {:keys [sid doc layout on-doc] :as sheet} dispatch! q]
  (let [url (:url doc)]
    [:div.doc-layers
     [:canvas (merge (pdf-canvas-attrs url q on-doc) layout)]
     [:div.textLayer (merge (text-layer-attrs url q sid dispatch!) layout {:replicant/key (str url "#" q)})]
     (marks db sheet dispatch! q)]))

(defn- html-layers
  "An HTML page: the page itself, the marks over it when near the page shown
  and the page is laid out (its text is there to find)."
  [db {:keys [sid doc]} sheet dispatch! q near?]
  (let [url    (:url doc)
        ready? (db/text-layer-ready? db url q)]
    [:div.doc-layers
     [:div.html-page (html-page-attrs url q (db/doc-html db url q) sid (and near? (not ready?)) dispatch!)]
     (when (and near? ready?) (marks db sheet dispatch! q))]))

(defn- doc-sheet
  "Page `q` of the source. A PDF page is drawn when near the page shown,
  else a blank sheet of the same size (the node's page carries its
  highlight); an HTML page is always there (its height is its text's), its
  marks only when near."
  [db {:keys [doc aspect hl-page found size] :as sheet} dispatch! q near?]
  (let [html? (= :html (:kind doc))]
    [:div.doc-sheet {:replicant/key (str (:url doc) "#" q) :data-page (str q) :class (when html? "html")
                     :style (merge size
                                   (when-let [a (and (not html?) (if (= q hl-page) (or (:aspect found) aspect) aspect))]
                                     {:aspect-ratio (str a)}))}
     (cond
       html?  (html-layers db sheet sheet dispatch! q near?)
       near?  (pdf-layers db sheet dispatch! q))]))

(defn- page-input
  "\"Page [p] / n\": type a page number and Enter to go there; Esc or
  leaving the box gives up. While typing, the box holds what is typed, not
  the page scrolled to."
  [db sid p n following? dispatch!]
  (let [draft (get-in db [:doc :draft])]
    [:form.page {:on {:submit (fn [e]
                                (.preventDefault e)
                                (dispatch! {:db #(db/doc-page-entered % sid)
                                            :history (if following? :push :replace)})
                                #?(:cljs (some-> (.querySelector (.-target e) "input") .blur)))}}
     [:label {:for "doc-page-input"} "Page"]
     [:input#doc-page-input
      {:type "text" :inputmode "numeric" :autocomplete "off"
       :aria-label (str "Page number" (when n (str ", of " n)))
       :size (max 2 (count (str (or n p))))
       :value (or draft (str p))
       :on {:focus #(.select (.-target %))
            :input #(dispatch! {:db (fn [d] (db/doc-page-typed d (.. % -target -value)))})
            :keydown #(when (= "Escape" (.-key %)) (.blur (.-target %)))
            :blur #(dispatch! {:db (fn [d] (db/doc-page-typed d nil))})}}]
     (when n [:span (str "/ " n)])]))

(defn source-pane
  "The original document, every page in one scrolling column, at the
  selected node's page with its source text highlighted."
  [db node dispatch!]
  (let [source (:node/source node)
        sid   (:source/id source)
        {:keys [url kind] :as doc} (source-doc source)
        html? (= :html kind)
        p     (db/doc-page db)
        n     (db/doc-pages db sid)
        key   (db/highlight-key db url)
        found (db/highlight db key)
        following? (db/following? db)
        ;; leaving the node's page for the source is one step back in history;
        ;; turning further pages replaces it
        goto  (fn [page] #(dispatch! {:db (fn [d] (db/doc-goto d sid page))
                                      :history (if following? :push :replace)}))
        ;; pages are drawn to fit the pane's width: an attribute that changes with it
        ;; re-runs the drawing hooks when the pane widens or narrows
        mode   (if (db/wide? db) "wide" "split")
        zoom   (db/doc-zoom db)
        ;; the panes' share too: pages drawn again when the divider is let go
        layout {:data-layout (str mode "@" zoom "@" (:split db))}
        sheet  {:sid sid :doc doc :node node :aspect (db/doc-aspect db sid) :hl-page (second key)
                :found found :layout layout
                ;; the pane's width x zoom; full width, no wider than reads well
                :size {:width (str (* 100 zoom) "%")
                       :max-width (when (db/wide? db) (str (* 1000 zoom) "px"))}
                :on-doc (fn [pages aspect]
                          (when (or (not= pages n) (nil? (db/doc-aspect db sid)))
                            (dispatch! {:db #(db/doc-loaded % sid pages aspect)})))}
        target (db/doc-target db p)
        sync   {:page p
                :y (or (:y target) (when (and following? found) (some :y (:rects found))))
                :key (or (:key target) (pr-str key))
                :layout (pr-str [url n mode zoom])}
        ;; an HTML page's text is there once its pages are loaded and it is laid out
        ready? (or (not html?) (db/text-layer-ready? db url (second key)))
        load-pages (when (and html? (not n))
                     #?(:cljs #(html/load! url (fn [pages] (dispatch! {:db (fn [d] (db/doc-html-loaded d sid url pages))})))
                        :clj  (constantly nil)))]
    [:aside.source
     [:div.doc-toolbar
      (panes/width-toggle db dispatch! "text")
      [:button {:disabled (<= p 1) :on {:click (goto (dec p))}} "‹"]
      (page-input db sid p n following? dispatch!)
      [:button {:disabled (boolean (and n (>= p n))) :on {:click (goto (inc p))}} "›"]
      (ideas-ui/bookmark-toggle db node dispatch!)
      (when-not following?
        [:button.follow {:on {:click #(dispatch! {:db db/doc-follow :history :push})}} "Back to selection"])]
     [:div.doc-pages (merge {:class (when html? "html")}
                            (pages-attrs doc load-pages ready? following? sync key (db/highlight-texts node) found dispatch!))
      ;; until the page count is known, only the page shown
      (for [q (if n (range 1 (inc n)) [p])]
        (doc-sheet db sheet dispatch! q (db/doc-window p q zoom)))]
     (t/zoom-controls [[:plus (str "Zoom in (" (Math/round (* 100 zoom)) "%)")
                        #(dispatch! {:db (fn [d] (db/doc-zoom-step d 1))}) (>= zoom (peek db/doc-zooms))]
                       [:minus (str "Zoom out (" (Math/round (* 100 zoom)) "%)")
                        #(dispatch! {:db (fn [d] (db/doc-zoom-step d -1))}) (<= zoom (first db/doc-zooms))]
                       [:width "Fit the width" #(dispatch! {:db (fn [d] (db/doc-zoomed d 1))}) (== zoom 1)]
                       (panes/fullscreen-button db dispatch!)])]))
