(ns robertluo.fly-learner.ui.core.views.pdf
  "The source document beside the reading (pdf.js, see
  robertluo.fly-learner.ui.pdf): every page in one scrolling column, the
  node's text highlighted, its key sentences and the learner's quotes
  marked. The DOM hooks are inert on the JVM."
  (:require [robertluo.fly-learner.ui.core.api :as api]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.highlight :as hl]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.panes :as panes]
            [robertluo.fly-learner.ui.core.views.template :as t]
            #?(:cljs [robertluo.fly-learner.ui.pdf :as pdf])
            #?(:cljs [robertluo.fly-learner.ui.select :as select])
            #?(:cljs [robertluo.fly-learner.ui.zoom :as zoom])))

(defn- pdf-canvas-attrs
  "Canvas attrs that (re)draw page `p` of `url` via pdf.js after each render.
  On the JVM (tests, no DOM) the canvas is inert."
  #?(:cljs [url p on-pages] :clj [_url _p _on-pages])
  #?(:cljs (t/on-render #(pdf/render-page! % url p on-pages))
     :clj  {}))

(defn- pages-attrs
  "Attrs of the column of pages: kept at the page shown, and at the node's
  highlight while following the node (`sync`, see pdf/sync-scroll!; an
  attribute too, so the hook runs when it changes); scrolling it turns the
  page, one step in history once it pauses. Finds the highlight `key` once
  (pdf.js text content). Inert on the JVM."
  #?(:cljs [following? sync key texts found dispatch!] :clj [following? sync _key _texts _found _dispatch!])
  (merge {:data-following (str (boolean following?)) :data-sync (pr-str sync)}
         #?(:cljs (t/on-render
                   (fn [pane]
                       (zoom/pinch! pane (fn [f] (dispatch! {:db #(db/pdf-zoomed % (* f (db/pdf-zoom %)))})))
                       (pdf/watch-scroll! pane
                                          (fn [p] (dispatch! {:db #(db/pdf-scrolled % p)}))
                                          ;; leaving the node's page is one step back in history;
                                          ;; the left pane then follows to the node on that page
                                          (fn [was-following?]
                                            (dispatch! {:history (if was-following? :push :replace)
                                                        :pull :page-sync})))
                       (when-not found
                         (pdf/ensure-highlight! key texts
                                                (fn [k result]
                                                  (dispatch! {:db #(db/highlight-found % k result)}))))
                       (pdf/sync-scroll! pane sync)))
            :clj  {})))

(defn- text-layer-attrs
  "The PDF page's text, selectable to react to (pdf.js TextLayer, filled
  after render); says when it is laid out. Inert on the JVM."
  #?(:cljs [url p source-id dispatch!] :clj [_url p source-id _dispatch!])
  (merge {:data-anchor "" :data-field "pdf" :data-source (str source-id) :data-page (str p)}
         #?(:cljs (t/on-render #(pdf/render-text-layer! % url p
                                                        (fn [] (dispatch! {:db (fn [d] (db/text-layer-ready d url p))}))))
            :clj  {})))

(defn- quote-marks-attrs
  "Marks layer attrs that find the PDF quotes keyed `k` in the page's text
  layer (its sibling) once that is laid out. Inert on the JVM."
  #?(:cljs [k quotes ready? placed? dispatch!] :clj [k _quotes ready? _placed? _dispatch!])
  (merge {:data-quotes (pr-str [k ready?])}
         #?(:cljs (t/on-render
                   (fn [^js node]
                       (when (and ready? (not placed?))
                         (when-let [layer (some-> (.-parentElement node) (.querySelector ".textLayer"))]
                           (let [placed (into {} (for [{:keys [idea anchor]} quotes
                                                       :let [rects (select/quote-rects layer anchor)]
                                                       :when rects]
                                                   [(:idea/id idea) rects]))]
                             ;; not inside this render
                             (js/setTimeout #(dispatch! {:db (fn [d] (db/pdf-quotes-placed d k placed))}) 0))))))
            :clj  {})))

(defn- pdf-quote-marks
  "Ideas on text selected on this PDF page, marked where that text is."
  [db sid url p dispatch!]
  (let [quotes (db/pdf-quotes db sid p)
        k      (db/pdf-quotes-key sid p quotes)
        placed (get-in db [:pdf :placed k])
        by-id  (into {} (map (juxt (comp :idea/id :idea) :idea)) quotes)]
    [:div.pdf-idea-marks (quote-marks-attrs k quotes (db/text-layer-ready? db url p)
                                            (db/pdf-quotes-placed? db k) dispatch!)
     (for [[id rects] placed
           :let [{:idea/keys [stance]} (by-id id)]
           :when stance
           [i {:keys [x y w h]}] (map-indexed vector rects)]
       [:div.idea-mark {:replicant/key (str id "-" i) :class (name stance)
                        :style {:left (t/pct x) :top (t/pct y) :width (t/pct w) :height (t/pct h)}}])]))

(defn- key-marks-attrs
  "Attrs that find the key sentences `kps` on page `p` once (pdf.js text
  content), unless placed. Inert on the JVM."
  #?(:cljs [url p kps placed? dispatch!] :clj [_url p kps _placed? _dispatch!])
  (merge {:data-keys (pr-str [p (count kps)])}
         #?(:cljs (t/on-render
                   (fn [_]
                       (when (and (seq kps) (not placed?))
                         (pdf/find-each! url p (mapv :quote kps)
                                         (fn [placed] (dispatch! {:db #(db/keypoints-placed % url p kps placed)}))))))
            :clj  {})))

(defn- pdf-key-marks
  "The key sentences on this page: tinted, their label in the page's margin."
  [db sid url p dispatch!]
  (let [kps    (db/page-keypoints db sid p)
        {:keys [found left] :as placed} (db/keypoint-rects db url p kps)
        margin (max 0.03 (- (or left 0.1) 0.012))
        found  (sort-by (comp :y first second) found)
        ;; labels close together stack rather than overlap
        tops   (hl/stack-labels (for [[{:keys [label]} rects] found] {:y (:y (first rects)) :label label})
                                (- margin 0.008) (or (db/pdf-aspect db sid) 0.75))]
    [:div.pdf-key-marks (key-marks-attrs url p kps (some? placed) dispatch!)
     (for [[i [{:keys [label quote]} rects] top] (map vector (range) found tops)]
       [:div {:replicant/key (str i quote)}
        [:div.key-label {:title label :style {:top (t/pct top) :width (t/pct (- margin 0.008))}} label]
        (for [[j {:keys [x y w h]}] (map-indexed vector rects)]
          [:div.key-mark {:replicant/key j :style {:left (t/pct x) :top (t/pct y) :width (t/pct w) :height (t/pct h)}}])])]))

(defn- open-bar-attrs
  "Attrs that find node `id`'s `texts` on page `p` once. Inert on the JVM."
  #?(:cljs [url p id texts pending? dispatch!] :clj [_url p _id texts _pending? _dispatch!])
  (merge {:data-open (pr-str [p (count texts)])}
         #?(:cljs (t/on-render
                   (fn [_]
                       (when (and pending? (seq texts))
                         (pdf/find-each! url p texts
                                         (fn [{:keys [rects]}]
                                           (dispatch! {:db #(db/open-placed % url p id texts rects)}))))))
            :clj  {})))

(defn- open-bar
  "What is open on the left, where it is on this page: a bar in the margin
  along it, on every page it covers."
  [db url {:node/keys [id] :as node} q dispatch!]
  (let [texts (db/open-texts node q)
        span  (when texts (db/open-span db url q id texts))]
    [:div.pdf-marks (open-bar-attrs url q id texts (= :pending span) dispatch!)
     (when (vector? span)
       (let [[top bottom left] span]
         [:div.open-bar {:title "Open on the left"
                         :style {:left (t/pct (max 0.004 (- left 0.022))) :top (t/pct top)
                                 :height (t/pct (- bottom top))}}]))]))

(defn- pdf-sheet
  "Page `q` of the source: drawn when near the page shown, else a blank
  sheet of the same size. The node's page carries its highlight."
  [db {:keys [sid url node aspect hl-page found layout on-doc size]} dispatch! q near?]
  [:div.pdf-sheet {:replicant/key (str url "#" q) :data-page (str q)
                   :style (merge size
                                 (when-let [a (if (= q hl-page) (or (:aspect found) aspect) aspect)]
                                   {:aspect-ratio (str a)}))}
   (when near?
     [:div.pdf-layers
      [:canvas (merge (pdf-canvas-attrs url q on-doc) layout)]
      [:div.textLayer (merge (text-layer-attrs url q sid dispatch!) layout {:replicant/key (str url "#" q)})]
      (pdf-key-marks db sid url q dispatch!)
      (pdf-quote-marks db sid url q dispatch!)
      ;; what is open on the left: a bar in the margin along it
      (open-bar db url node q dispatch!)])])

(defn- page-input
  "\"Page [p] / n\": type a page number and Enter to go there; Esc or
  leaving the box gives up. While typing, the box holds what is typed, not
  the page scrolled to."
  [db sid p n following? dispatch!]
  (let [draft (get-in db [:pdf :draft])]
    [:form.page {:on {:submit (fn [e]
                                (.preventDefault e)
                                (dispatch! {:db #(db/pdf-page-entered % sid)
                                            :history (if following? :push :replace)})
                                #?(:cljs (some-> (.querySelector (.-target e) "input") .blur)))}}
     [:label {:for "pdf-page-input"} "Page"]
     [:input#pdf-page-input
      {:type "text" :inputmode "numeric" :autocomplete "off"
       :aria-label (str "Page number" (when n (str ", of " n)))
       :size (max 2 (count (str (or n p))))
       :value (or draft (str p))
       :on {:focus #(.select (.-target %))
            :input #(dispatch! {:db (fn [d] (db/pdf-page-typed d (.. % -target -value)))})
            :keydown #(when (= "Escape" (.-key %)) (.blur (.-target %)))
            :blur #(dispatch! {:db (fn [d] (db/pdf-page-typed d nil))})}}]
     (when n [:span (str "/ " n)])]))

(defn source-pane
  "The original document (pdf.js), every page in one scrolling column, at
  the selected node's page with its source text highlighted."
  [db node dispatch!]
  (let [sid   (get-in node [:node/source :source/id])
        url   (api/source-url sid)
        p     (db/pdf-page db)
        n     (db/pdf-pages db sid)
        key   (db/highlight-key db url)
        found (db/highlight db key)
        following? (db/following? db)
        ;; leaving the node's page for the source is one step back in history;
        ;; turning further pages replaces it
        goto  (fn [page] #(dispatch! {:db (fn [d] (db/pdf-goto d sid page))
                                      :history (if following? :push :replace)}))
        ;; pages are drawn to fit the pane's width: an attribute that changes with it
        ;; re-runs the drawing hooks when the pane widens or narrows
        mode   (if (db/wide? db) "wide" "split")
        zoom   (db/pdf-zoom db)
        ;; the panes' share too: pages drawn again when the divider is let go
        layout {:data-layout (str mode "@" zoom "@" (:split db))}
        sheet  {:sid sid :url url :node node :aspect (db/pdf-aspect db sid) :hl-page (second key)
                :found found :layout layout
                ;; the pane's width x zoom; full width, no wider than reads well
                :size {:width (str (* 100 zoom) "%")
                       :max-width (when (db/wide? db) (str (* 1000 zoom) "px"))}
                :on-doc (fn [pages aspect]
                          (when (or (not= pages n) (nil? (db/pdf-aspect db sid)))
                            (dispatch! {:db #(db/pdf-loaded % sid pages aspect)})))}
        target (db/pdf-target db p)
        sync   {:page p
                :y (or (:y target) (when (and following? found) (some :y (:rects found))))
                :key (or (:key target) (pr-str key))
                :layout (pr-str [url n mode zoom])}]
    [:aside.source
     [:div.pdf-toolbar
      (panes/width-toggle db dispatch! "text")
      [:button {:disabled (<= p 1) :on {:click (goto (dec p))}} "‹"]
      (page-input db sid p n following? dispatch!)
      [:button {:disabled (boolean (and n (>= p n))) :on {:click (goto (inc p))}} "›"]
      (ideas-ui/bookmark-toggle db node dispatch!)
      (when-not following?
        [:button.follow {:on {:click #(dispatch! {:db db/pdf-follow :history :push})}} "Back to selection"])]
     [:div.pdf-page (pages-attrs following? sync key (db/highlight-texts node) found dispatch!)
      ;; until the page count is known, only the page shown
      (for [q (if n (range 1 (inc n)) [p])]
        (pdf-sheet db sheet dispatch! q (db/pdf-window p q zoom)))]
     (t/zoom-controls [[:plus (str "Zoom in (" (Math/round (* 100 zoom)) "%)")
                        #(dispatch! {:db (fn [d] (db/pdf-zoom-step d 1))}) (>= zoom (peek db/pdf-zooms))]
                       [:minus (str "Zoom out (" (Math/round (* 100 zoom)) "%)")
                        #(dispatch! {:db (fn [d] (db/pdf-zoom-step d -1))}) (<= zoom (first db/pdf-zooms))]
                       [:width "Fit the width" #(dispatch! {:db (fn [d] (db/pdf-zoomed d 1))}) (== zoom 1)]
                       (panes/fullscreen-button db dispatch!)])]))
