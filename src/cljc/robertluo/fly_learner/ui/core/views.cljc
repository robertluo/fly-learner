(ns robertluo.fly-learner.ui.core.views
  "UI views: Replicant hiccup. Components receive db and dispatch!, and
  dispatch effect maps ({:db f :pull op :history :push})."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.history :as history]
            #?(:cljs [robertluo.fly-learner.ui.pdf :as pdf])))

;;=============================================================================
;; Helpers
;;=============================================================================

(def ^:private kind-label
  {:source "Overview" :section "Section" :group "Passage" :paragraph "Source text"})

(defn- link
  "An <a> with a real href (open in new tab works) whose plain click is
  handled in-app by dispatching `effects`."
  [dispatch! effects path attrs & children]
  (into [:a (merge {:href path
                    :on {:click (fn [e]
                                  (when-not (or (.-metaKey e) (.-ctrlKey e) (.-shiftKey e))
                                    (.preventDefault e)
                                    (dispatch! effects)))}}
                   attrs)]
        children))

(defn- open-node-effects [id]
  {:db #(db/open-node % id) :pull [:node :ideas] :history :push})

(defn- node-link
  ([dispatch! node] (node-link dispatch! node {} (:node/title node)))
  ([dispatch! {:node/keys [id]} attrs & children]
   (apply link dispatch! (open-node-effects id) (str "/n/" id) attrs children)))

(defn- keyword-link [dispatch! kw]
  (link dispatch! {:db #(db/open-keyword % kw) :pull :keyword :history :push}
        (history/state->path {:view :keyword :keyword kw})
        {:class ["chip" "keyword"] :title "Keyword (names no concept)"}
        kw))

(defn- concept-link
  "Link to a concept page; the gloss shows on hover."
  [dispatch! {:concept/keys [id label gloss]} attrs & children]
  (apply link dispatch! {:db #(db/open-concept % id) :pull [:concept :concept-graph :learning :ideas] :history :push
                         :animate true}
         (history/state->path {:view :concept :concept-id id})
         (merge {:title (or gloss label)} attrs)
         (or (seq children) [label])))

(defn- concept-chip [dispatch! c & extra]
  (apply concept-link dispatch! c {:class ["chip" "concept"] :replicant/key (:concept/id c)}
         (:concept/label c) extra))

(def ^:private concepts-effects {:db db/open-concepts :pull :concept-index :history :push})

(def ^:private study-effects {:db db/open-study :pull :learning :history :push})

(defn- quiz-effects
  "Quiz on `item` (nil: what to study next)."
  [item]
  {:db #(db/quiz-start % item) :pull [:question-create :learning] :history :push})

(defn percent
  "A score as a percentage: whole numbers, one decimal below 10%."
  [x]
  (let [p (* 100 (or x 0))]
    (if (and (pos? p) (< p 10))
      (str (/ (Math/round (* 10.0 p)) 10.0) "%")
      (str (Math/round (double p)) "%"))))

(defn- score-bar [score]
  [:span.score-bar [:span.fill {:style {:width (str (* 100 (or score 0)) "%")}}]])

(defn- meta-line [{:node/keys [kind page]}]
  [:span.meta (kind-label kind) (when page (str " · p. " page))])

;;=============================================================================
;; Pieces
;;=============================================================================

(defn- ask-effects
  "Ask `q`: show it, search at once, answer with the model."
  [q]
  {:db #(db/ask-submitted % q) :pull [:search :ask-create]})

(defn- submit-ask
  "Form submit: ask the form's `q` input, then clear it."
  #?(:cljs [dispatch! e] :clj [_dispatch! _e])
  #?(:cljs (do (.preventDefault e)
               (let [input (.. e -target -elements -q)
                     q     (str/trim (.-value input))]
                 (when-not (str/blank? q)
                   (set! (.-value input) "")
                   (.blur input)
                   (dispatch! (ask-effects q)))))
     :clj  nil))

(defn- header [{:keys [asking] :as db} dispatch!]
  [:header.top
   (link dispatch! {:db db/go-home :history :push} "/" {:class "brand"} "Fly Learner")
   [:nav.top-nav
    (link dispatch! concepts-effects "/c" {} "Concepts")
    (link dispatch! study-effects "/q" {} "Study"
          (when-let [o (:overall (:learning db))] [:span.count (percent o)]))]
   [:form.ask {:on {:submit #(submit-ask dispatch! %)}}
    [:input {:name "q" :type "search" :autocomplete "off" :disabled (boolean asking)
             :placeholder (if asking "Answering…" "Ask the knowledge base…")}]]])

(defn- error-banner [{:keys [error]} dispatch!]
  (when error
    [:div.error
     [:span (or (:reason error) (name (:code error)))]
     [:button {:on {:click #(dispatch! {:db db/clear-error})}} "Dismiss"]]))

(declare anchored-text)

(defn- card
  "A node in a list: title link, kind/page, summary or source text. With
  `in` ({:db .. :source-id ..}), source text can be selected to react to,
  and shows the quotes of ideas on it."
  ([dispatch! n] (card dispatch! n nil))
  ([dispatch! {:node/keys [id summary text kind] :as n} in]
   [:li.card {:replicant/key id}
    (node-link dispatch! n {:class "card-title"}
               (if (= :paragraph kind) (meta-line n) (:node/title n)))
    (when-not (= :paragraph kind) (meta-line n))
    (cond
      (and (= :paragraph kind) in) (anchored-text :p.source-text in id :text text)
      (= :paragraph kind)          [:p.source-text text]
      summary                      [:p.preview summary])]))

;;=============================================================================
;; Ideas
;;=============================================================================

(def ^:private stance-mark {:agree "✓" :disagree "✗" :note "✎"})
(def ^:private stance-label {:agree "Agree" :disagree "Disagree" :note "Note"})

(defn- create-idea-op [data]
  {:pattern {:ideas {nil data}} :then (fn [r] {:db #(db/idea-added % (get r 'ideas))})})

(defn- delete-idea-op [id]
  {:pattern {:ideas {{:idea/id id} nil}} :then (fn [_] {})})

(defn- anchored-text
  "Element `tag` with `text` of node `id`'s `field`: selectable to react to
  (data-anchor, see robertluo.fly-learner.ui.select), with the quotes of
  ideas on it marked."
  [tag {:keys [db source-id]} id field text]
  (let [marks (db/quote-marks db id field)]
    [tag {:data-anchor "" :data-node (str id) :data-field (name field) :data-source (str source-id)}
     (if (empty? marks)
       text
       (for [[i [s m]] (map-indexed vector (ideas/mark-segments text marks))]
         (if m
           [:mark {:replicant/key i :class (name (:idea/stance m))
                   :title (or (:idea/text m) (stance-label (:idea/stance m)))} s]
           [:span {:replicant/key i} s])))]))

(defn- snippet-of [s n] (when s (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- composer
  "The note being written, with ideas it may link to."
  [db dispatch!]
  (let [{:keys [anchors text links]} (get-in db [:ideas :draft])
        others (->> (get-in db [:ideas :all]) (filter :idea/text) (take-last 10) reverse)]
    [:div.composer
     [:textarea {:value text :rows 3 :placeholder "Your idea, in your own words…"
                 :on {:input #(dispatch! {:db (fn [d] (db/draft-typed d (.. % -target -value)))})}}]
     (when (seq others)
       [:div.link-picker
        [:span.muted "Links to "]
        (for [{:idea/keys [id stance text]} others]
          [:button.chip {:replicant/key (str id)
                         :class (when (contains? links id) "on")
                         :on {:click #(dispatch! {:db (fn [d] (db/draft-link d id))})}}
           (stance-mark stance) " " (snippet-of text 40)])])
     [:div.actions
      [:button.btn.primary {:disabled (str/blank? text)
                            :on {:click #(dispatch! {:pull (create-idea-op {:idea/stance :note :idea/text text
                                                                            :idea/anchors anchors
                                                                            :idea/links (vec links)})})}}
       "Save note"]
      [:button.btn.quiet {:on {:click #(dispatch! {:db db/draft-cancel})}} "Cancel"]]]))

(defn- reaction-buttons
  "Agree / disagree with `anchor`: one at most, clicking again takes it back."
  [db dispatch! anchor]
  (let [mine (db/reaction db anchor)]
    (for [stance [:agree :disagree]
          :let [on? (= stance (:idea/stance mine))]]
      [:button.btn.react {:replicant/key (name stance) :class [(name stance) (when on? "on")]
                          :on {:click #(dispatch! (cond-> {:pull []}
                                                    mine (-> (assoc :db (fn [d] (db/idea-removed d (:idea/id mine))))
                                                             (update :pull conj (delete-idea-op (:idea/id mine))))
                                                    (not on?) (update :pull conj (create-idea-op {:idea/stance stance
                                                                                                  :idea/anchors [anchor]}))))}}
       (stance-mark stance) " " (stance-label stance)])))

(defn- idea-item [db dispatch! {:idea/keys [id stance text anchors links targets]} show-target?]
  (let [by-id (into {} (map (juxt :idea/id identity)) (get-in db [:ideas :all]))]
    [:li.idea {:replicant/key (str id) :class (name stance)}
     [:span.stance {:title (stance-label stance)} (stance-mark stance)]
     [:div.idea-body
      (for [[i a] (map-indexed vector anchors) :when (:quote a)]
        [:blockquote {:replicant/key i} (snippet-of (:quote a) 300)])
      (if text [:p text] [:p.muted (stance-label stance)])
      (when show-target?
        [:span.meta "On " (str/join ", " (map :title targets))])
      (when (seq links)
        [:span.links "↔ " (str/join " · " (keep #(snippet-of (:idea/text (by-id %)) 50) links))])]
     [:button.btn.quiet.remove {:title "Remove" :on {:click #(dispatch! {:db (fn [d] (db/idea-removed d id))
                                                                        :pull (delete-idea-op id)})}}
      "×"]]))

(defn- ideas-panel
  "Your ideas on `anchor` (and on `ideas` about things inside it)."
  [db dispatch! anchor ideas]
  (let [draft (get-in db [:ideas :draft])]
    [:section.ideas
     [:h2 "Your ideas"]
     [:div.reactions
      (reaction-buttons db dispatch! anchor)
      [:button.btn {:on {:click #(dispatch! {:db (fn [d] (db/draft-start d [anchor] false))})}} "✎ Add a note"]]
     (when (and draft (not (:from-selection? draft)) (= [anchor] (:anchors draft)))
       (composer db dispatch!))
     (let [shown (remove #(and (nil? (:idea/text %)) (= [anchor] (:idea/anchors %))) ideas)]
       (if (seq shown)
         [:ul.idea-list (for [i (reverse shown)]
                          (idea-item db dispatch! i (not= [anchor] (:idea/anchors i))))]
         [:p.hint.muted "Agree, disagree or add a note. Select any sentence to react to just that."]))]))

(defn- selection-popover
  "Selected text, offered a reaction."
  [db dispatch!]
  (when-let [{:keys [anchor x y]} (get-in db [:ideas :selection])]
    (let [draft (get-in db [:ideas :draft])]
      [:div.idea-popover {:style {:left (str (max 8 (- x 160)) "px") :top (str (+ y 8) "px")}}
       [:p.quote-preview "“" (snippet-of (:quote anchor) 120) "”"]
       (if (:from-selection? draft)
         (composer db dispatch!)
         [:div.actions
          (for [stance [:agree :disagree]]
            [:button.btn {:replicant/key (name stance) :class (name stance)
                          :on {:click #(dispatch! {:pull (create-idea-op {:idea/stance stance :idea/anchors [anchor]})})}}
             (stance-mark stance) " " (stance-label stance)])
          [:button.btn {:on {:click #(dispatch! {:db (fn [d] (db/draft-start d [anchor] true))})}} "✎ Note"]
          [:button.btn.quiet {:on {:click #(dispatch! {:db db/draft-cancel})}} "×"]])])))

;;=============================================================================
;; Views
;;=============================================================================

(defn- ask-link [dispatch! {:ask/keys [id]} attrs & children]
  (apply link dispatch! {:db #(db/open-ask % id) :pull :ask :history :push} (str "/a/" id) attrs children))

(defn- home-view [{:keys [sources loading? ask-history]} dispatch!]
  [:main.single
   [:h1 "Sources"]
   (cond
     (seq sources)
     [:ul.cards
      (for [{:source/keys [id title root]} sources]
        [:li.card {:replicant/key id}
         (node-link dispatch! root {:class "card-title"} title)
         [:p.preview (:node/summary root)]])]
     loading? [:p.muted "Loading…"]
     :else    [:p.muted "No sources yet. Ingest a PDF from the REPL (see dev/user.clj)."])
   (when (seq ask-history)
     [:section
      [:h2 "Recent questions"]
      [:ul.cards.compact
       (for [{:ask/keys [id question title] :as a} (take 8 ask-history)]
         [:li.card {:replicant/key id}
          (ask-link dispatch! a {:class "card-title"} question)
          [:p.preview title]])]])])

(defn- node-article [db node dispatch!]
  (let [{:node/keys [id title summary text keywords concepts children links ancestors kind source]} node
        leaves? (every? #(= :paragraph (:node/kind %)) children)
        in      {:db db :source-id (:source/id source)}]
    [:article.node
     (when (seq ancestors)
       [:nav.crumbs
        (for [a ancestors]
          [:span {:replicant/key (:node/id a)} (node-link dispatch! a) " › "])])
     (meta-line node)
     [:h1 title]
     (when summary (anchored-text :p.summary in id :summary summary))
     (when (and text (= :paragraph kind)) (anchored-text :p.source-text.full in id :text text))
     (when (or (seq concepts) (seq keywords))
       [:div.keywords
        (for [c concepts] (concept-chip dispatch! c))
        (for [kw keywords] [:span {:replicant/key kw} (keyword-link dispatch! kw)])])
     (when (seq children)
       [:section
        [:h2 (if leaves? "Source text" "Drill down")]
        [:ol.cards (for [c children] (card dispatch! c in))]])
     (ideas-panel db dispatch! (db/node-anchor node)
                  (db/ideas-in-node db id [:page (:source/id source) (db/pdf-page db)]))
     (when (seq links)
       [:section
        [:h2 "Related"]
        [:ul.cards.compact (for [l links] (card dispatch! l))]])]))

(defn- pdf-canvas-attrs
  "Canvas attrs that (re)draw page `p` of `url` via pdf.js after each render.
  On the JVM (tests, no DOM) the canvas is inert."
  #?(:cljs [url p on-pages] :clj [_url _p _on-pages])
  #?(:cljs {:replicant/on-render (fn [{:replicant/keys [node]}] (pdf/render-page! node url p on-pages))}
     :clj  {}))

(defn- highlight-attrs
  "Sheet attrs that find the highlight for `key` once (pdf.js text content),
  then scroll the pane to it."
  #?(:cljs [key texts found dispatch!] :clj [_key _texts _found _dispatch!])
  #?(:cljs {:replicant/on-render
            (fn [{:replicant/keys [node]}]
              (if found
                (pdf/scroll-to-highlight! node key (:rects found))
                (pdf/ensure-highlight! key texts
                                       (fn [k result]
                                         (dispatch! {:db #(db/highlight-found % k result)})))))}
     :clj  {}))

(defn- text-layer-attrs
  "The PDF page's text, selectable to react to (pdf.js TextLayer, filled
  after render); inert on the JVM."
  #?(:cljs [url p source-id] :clj [_url p source-id])
  (merge {:data-anchor "" :data-field "pdf" :data-source (str source-id) :data-page (str p)}
         #?(:cljs {:replicant/on-render (fn [{:replicant/keys [node]}] (pdf/render-text-layer! node url p))}
            :clj  {})))

(defn- pct [x] (str (* 100 x) "%"))

(defn- source-pane
  "The original document (pdf.js), on the selected node's page, with the
  node's source text highlighted."
  [db node dispatch!]
  (let [sid   (get-in node [:node/source :source/id])
        url   (str "/files/sources/" sid)
        p     (db/pdf-page db)
        n     (db/pdf-pages db sid)
        key   (db/highlight-key db url)
        found (db/highlight db key)
        goto  (fn [page] #(dispatch! {:db (fn [d] (db/pdf-goto d sid page))}))]
    [:aside.source
     [:div.pdf-toolbar
      [:button {:disabled (<= p 1) :on {:click (goto (dec p))}} "‹"]
      [:span.page "Page " p (when n (str " / " n))]
      [:button {:disabled (boolean (and n (>= p n))) :on {:click (goto (inc p))}} "›"]
      (when-not (db/following? db)
        [:button.follow {:on {:click #(dispatch! {:db db/pdf-follow})}} "Back to selection"])]
     [:div.pdf-page
      [:div.pdf-sheet (cond-> (highlight-attrs key (db/highlight-texts node) found dispatch!)
                        found (assoc :style {:aspect-ratio (str (:aspect found))}))
       [:canvas (pdf-canvas-attrs url p (fn [pages]
                                          (when (not= pages n)
                                            (dispatch! {:db #(db/pdf-loaded % sid pages)}))))]
       [:div.textLayer (assoc (text-layer-attrs url p sid) :replicant/key (str url "#" p))]
       [:div.pdf-marks
        (for [[i {:keys [x y w h]}] (map-indexed vector (:rects found))]
          [:div.mark {:replicant/key i
                      :style {:left (pct x) :top (pct y) :width (pct w) :height (pct h)}}])]]]]))

(defn- node-view [db dispatch!]
  (if-let [node (db/current-node db)]
    [:main.split
     [:section.content (node-article db node dispatch!)]
     (source-pane db node dispatch!)]
    [:main.single [:p.muted (if (:loading? db) "Loading…" "Not found.")]]))

(defn- keyword-view [{:keys [keyword keyword-results loading?]} dispatch!]
  (let [nodes (get keyword-results keyword)]
    [:main.single
     [:span.meta "Keyword"]
     [:h1 keyword]
     (cond
       (seq nodes) [:ul.cards (for [n nodes] (card dispatch! n))]
       loading?    [:p.muted "Loading…"]
       :else       [:p.muted "No nodes carry this keyword."])]))

;;=============================================================================
;; Ask
;;=============================================================================

(defn answer-parts
  "Split answer text into [:text s] and [:cite n] parts."
  [s]
  (for [tok (re-seq #"\[\d+\]|[^\[]+|\[" s)]
    (if-let [[_ n] (re-matches #"\[(\d+)\]" tok)]
      [:cite (parse-long n)]
      [:text tok])))

(defn- path-line [{:node/keys [path kind page]}]
  [:span.meta (str/join " › " (map :node/title (rest path)))
   (when (seq (rest path)) " · ")
   (kind-label kind) (when page (str " · p. " page))])

(defn- answer-view [{:ask/keys [answer cited]} dispatch!]
  [:div.answer
   (for [[i para] (map-indexed vector (str/split answer #"\n\s*\n"))]
     [:p {:replicant/key i}
      (for [[j [kind v]] (map-indexed vector (answer-parts para))]
        (if (= :cite kind)
          (if-let [n (get cited (dec v))]
            (node-link dispatch! n {:class "cite" :title (:node/title n) :replicant/key j} (str v))
            [:span.cite.gone {:replicant/key j :title "No longer in the knowledge base"} (str v)])
          [:span {:replicant/key j} v]))])])

(defn- snippet [s n]
  (when s (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- subgraph-tree
  "The cited nodes with the paths that lead to them, as a nested outline."
  [dispatch! nodes]
  [:ul.tree
   (for [{:node/keys [id kind title summary text] :keys [cite children] :as n} nodes]
     [:li {:replicant/key id :class (when cite "cited")}
      [:div.tree-row
       (when cite [:span.cite-num (str cite)])
       (node-link dispatch! n {:class "tree-title"}
                  (if (= :paragraph kind) (snippet text 90) title))
       (meta-line n)]
      (when cite
        (if (= :paragraph kind)
          [:p.source-text (snippet text 600)]
          (when summary [:p.preview summary])))
      (when (seq children) (subgraph-tree dispatch! children))])])

(defn- hit-cards [dispatch! hits]
  [:ul.cards.compact
   (for [{:node/keys [id kind title summary text] :as n} hits]
     [:li.card {:replicant/key id}
      (node-link dispatch! n {:class "card-title"} (if (= :paragraph kind) (snippet text 120) title))
      (path-line n)
      (when (not= :paragraph kind) [:p.preview summary])])])

(defn- ask-view [{:keys [asking search-hits loading?] :as db} dispatch!]
  (let [ask      (db/current-ask db)
        question (or (:ask/question ask) asking)]
    [:main.single.ask
     [:span.meta "Question"]
     [:h1 question]
     (cond
       ask
       (list
        (answer-view ask dispatch!)
        (when (seq (:ask/followups ask))
          [:div.followups {:replicant/key "followups"}
           (for [q (:ask/followups ask)]
             [:button.chip {:replicant/key q :on {:click #(dispatch! (ask-effects q))}} q])])
        (when (seq (:ask/tree ask))
          [:section {:replicant/key "tree"}
           [:h2 "In the knowledge base"]
           (subgraph-tree dispatch! (:ask/tree ask))])
        (when (seq (:ask/matches ask))
          [:section {:replicant/key "matches"}
           [:h2 "Also matched"]
           (hit-cards dispatch! (:ask/matches ask))]))

       asking
       (list
        [:p.muted.thinking {:replicant/key "thinking"} "Answering from the knowledge base…"]
        (when-let [hits (seq (get search-hits asking))]
          [:section {:replicant/key "hits"}
           [:h2 "Matches"]
           (hit-cards dispatch! hits)]))

       loading? [:p.muted "Loading…"]
       :else    [:p.muted "Not found."])]))

;;=============================================================================
;; Study
;;=============================================================================

(defn- submit-answer [dispatch!]
  (dispatch! {:db db/quiz-submitted :pull :answer-create}))

(defn- quiz-panel [{{:keys [status question response result]} :quiz} dispatch!]
  (let [{:keys [title] :question/keys [prompt]} question]
    [:section.quiz
     (case status
       :asking
       [:p.muted.thinking "Writing a question"]

       (:answering :grading)
       (list
        [:span.meta {:replicant/key "m"} "Question · " title]
        [:p.prompt {:replicant/key "p"} prompt]
        [:textarea {:replicant/key "t"
                    :value response :rows 6 :placeholder "Answer in your own words…"
                    :disabled (= :grading status)
                    :on {:input   #(dispatch! {:db (fn [d] (db/quiz-typed d (.. % -target -value)))})
                         :keydown #(when (and (= "Enter" (.-key %)) (or (.-metaKey %) (.-ctrlKey %))
                                              (not (str/blank? response)))
                                     (submit-answer dispatch!))}}]
        (if (= :grading status)
          [:p.muted.thinking {:replicant/key "g"} "Grading"]
          [:div.actions {:replicant/key "a"}
           [:button.btn.primary {:disabled (str/blank? response) :on {:click #(submit-answer dispatch!)}
                                 :title "⌘/Ctrl + Enter"}
            "Submit"]
           [:button.btn {:on {:click #(dispatch! {:db (fn [d] (-> d (db/quiz-typed "") db/quiz-submitted))
                                                  :pull :answer-create})}
                         :title "Counts as a wrong answer: honest scores pick better questions"}
            "I don't know"]
           [:button.btn.quiet {:on {:click #(dispatch! {:db db/quiz-close})}} "Skip"]]))

       :graded
       (let [{:keys [grade feedback before after] :question/keys [answer points]} result]
         (list
          [:span.meta {:replicant/key "m"} "Question · " title]
          [:p.prompt {:replicant/key "p"} prompt]
          [:div.graded {:replicant/key "g"}
           [:span.grade (percent grade)] (score-bar grade)]
          [:p.feedback {:replicant/key "f"} feedback]
          (when-not (str/blank? response)
            [:details {:replicant/key "y"} [:summary "Your answer"] [:p.source-text response]])
          [:details {:replicant/key "x" :open (< grade 0.8)}
           [:summary "Model answer"]
           [:p answer]
           [:ul.points (for [[i pt] (map-indexed vector points)] [:li {:replicant/key i} pt])]]
          [:p.change {:replicant/key "c"}
           title " " (percent (:item before)) " → " [:strong (percent (:item after))]
           [:span.muted " · whole graph "] (percent (:overall before)) " → " [:strong (percent (:overall after))]]
          [:div.actions {:replicant/key "a"}
           [:button.btn.primary {:on {:click #(dispatch! (quiz-effects nil))}} "Next question"]
           [:button.btn.quiet {:on {:click #(dispatch! {:db db/quiz-close})}} "Done"]]))

       nil)]))

(defn- study-view [{:keys [learning quiz loading?] :as db} dispatch!]
  (let [{:keys [overall answers next]} learning]
    [:main.single.study
     [:span.meta "Study"]
     [:h1 "What you know"]
     (if learning
       [:div.overall-score
        [:span.big (percent overall)] (score-bar overall)
        [:p.muted "of the whole knowledge graph: every concept and every relation between two, "
         "weighted by how much the sources discuss it. Scores fade as you forget; a review brings them back. "
         (when (pos? answers) (str answers (if (= 1 answers) " answer" " answers") " so far."))]]
       (when loading? [:p.muted "Loading…"]))
     (when (:status quiz) (quiz-panel db dispatch!))
     (when (and (seq next) (not (#{:asking :answering :grading} (:status quiz))))
       [:section
        [:h2 "Study next"]
        [:p.hint.muted "What raises your score most: important, not yet known, and built on what you already know."]
        [:ul.cards.compact.next
         (for [{:keys [item title about score]} next]
           [:li.card {:replicant/key (str item)}
            [:div.next-head
             [:span.card-title title]
             (score-bar score)
             [:button.btn.primary {:on {:click #(dispatch! (quiz-effects item))}} "Quiz me"]]
            [:p.preview about]])]])]))

;;=============================================================================
;; Concepts
;;=============================================================================

(defn- provenance [by]
  (cond (= "human" by) "Curated"
        by             (str "Drafted by " by)))

(defn- concept-article
  [db {:concept/keys [id label gloss aliases by mentions]} score dispatch!]
  [:article.concept
   [:nav.crumbs (link dispatch! concepts-effects "/c" {} "Concepts") " ›"]
   [:span.meta "Concept · " (count mentions) (if (= 1 (count mentions)) " passage" " passages")
    (when-let [p (provenance by)] (str " · " p))]
   [:h1 label]
   (when gloss [:p.summary gloss])
   (when (seq aliases) [:p.aliases [:span.muted "Also written "] (str/join ", " aliases)])
   [:div.known
    [:span.muted "You know this"] (score-bar score) [:span.pct (percent score)]
    [:button.btn.primary {:on {:click #(dispatch! (quiz-effects (db/concept-item id)))}} "Quiz me on this"]]
   (ideas-panel db dispatch! (db/concept-anchor id) (db/ideas-about-concept db id))
   (when (seq mentions)
     [:section
      [:h2 "Mentioned in"]
      (hit-cards dispatch! mentions)])])

(defn- translate [x y] (str "translate(" x "," y ")"))

(defn- graph-node
  [dispatch! scale {:keys [id label gloss mentions x y r focus? selected? expanded? hidden score]}]
  [:g {:replicant/key id
       :class (cond-> ["cnode"]
                focus?    (conj "focus")
                selected? (conj "selected")
                expanded? (conj "expanded"))
       :transform (translate x y)
       :on {:click (fn [_] (dispatch! {:db #(db/graph-click % id) :pull :concept
                                       :history :push :animate true}))}}
   [:title (str label (when gloss (str ": " gloss)) " (" mentions " passages; you know " (percent score) ")")]
   (when selected? [:circle.halo {:r (+ r 5)}])
   [:circle.dot {:r r}]
   ;; the heat: how well it is known
   (when (pos? score) [:circle.heat {:r r :style {:fill-opacity (+ 0.15 (* 0.85 score))}}])
   [:text.label {:y (+ r (* 14 scale)) :text-anchor "middle" :font-size (* 12 scale)} label]
   (when (or expanded? (pos? hidden))
     [:g.badge {:transform (str (translate (* 0.75 r) (* -0.75 r)) " scale(" scale ")")
                :on {:click (fn [e]
                              (.stopPropagation e)
                              (dispatch! {:db #(db/graph-toggle % id) :animate true}))}}
      [:title (if expanded? "Collapse" (str "Expand: " hidden " more"))]
      [:circle {:r 8}]
      [:text {:dy "0.35em" :text-anchor "middle"} (if expanded? "−" "+")]])])

(defn- graph-edge [{:keys [from to kind weight shared x1 y1 x2 y2 score]}]
  [:line {:replicant/key (str from "-" to)
          :class (cond-> ["edge" (name kind)] (pos? score) (conj "known"))
          :x1 x1 :y1 y1 :x2 x2 :y2 y2
          :style (if (pos? score)
                   {:stroke-width (+ 2 (* 4 score)) :stroke-opacity (+ 0.4 (* 0.6 score))}
                   (when (= :related kind) {:stroke-opacity (+ 0.25 (min 0.6 (* 1.5 (or weight 0))))}))}
   [:title (str (if (= :broader kind) "is a kind or part of" (str shared " shared passages"))
                "; you know " (percent score))]])

(defn- concept-graph [db dispatch!]
  (let [{:keys [nodes edges box scale] {idea-dots :ideas idea-links :links} :ideas} (db/graph-scene db)
        id (:concept-id db)]
    [:aside.source.graph-pane
     [:div.graph-toolbar
      [:span (count nodes) (if (= 1 (count nodes)) " concept" " concepts")]
      [:span.legend
       [:span.key.broader] "broader above" [:span.key.related] "related"
       [:span.key.heat] "known"]
      (when-let [o (:overall (:learning db))]
        [:span.overall {:title "Of the whole graph, weighted by importance"} "You know " (percent o)])
      [:label.toggle [:input {:type "checkbox" :checked (boolean (get-in db [:ideas :layer?]))
                              :on {:change #(dispatch! {:db db/ideas-layer-toggle})}}]
       "My ideas"]
      [:button {:on {:click #(dispatch! {:db (fn [d] (db/graph-reset d id)) :animate true})}
                :title "Show only this concept and its neighbours"}
       "Reset"]]
     (if (seq nodes)
       [:svg.cgraph {:viewBox (str/join " " box) :preserveAspectRatio "xMidYMid meet"}
        [:g.edges (map graph-edge edges)]
        [:g.nodes (for [n nodes] (graph-node dispatch! scale n))]
        [:g.ideas
         (for [[i [[x1 y1] [x2 y2]]] (map-indexed vector idea-links)]
           [:line.idea-link {:replicant/key i :x1 x1 :y1 y1 :x2 x2 :y2 y2}])
         (for [{{:idea/keys [id stance text]} :idea x :x y :y} idea-dots]
           [:g.idea-dot {:replicant/key (str id) :class (name stance)
                         :transform (str (translate x y) " scale(" scale ")")}
            [:title (str (stance-label stance) (when text (str ": " text)))]
            [:rect {:x -4.5 :y -4.5 :width 9 :height 9 :transform "rotate(45)"}]])]]
       [:p.muted.graph-empty "Loading the concept graph…"])
     [:p.graph-hint "Click a concept to open and expand it, click it again to collapse. "
      "+ / − expand or collapse without opening. Filled: how well you know it."]]))

(defn- concept-view [db dispatch!]
  [:main.split
   [:section.content
    (if-let [c (db/current-concept db)]
      (concept-article db c (db/item-score db (db/concept-item (:concept/id c))) dispatch!)
      [:p.muted (if (:loading? db) "Loading…" "Not found.")])]
   (concept-graph db dispatch!)])

(defn- concepts-view [{:keys [concept-index concept-filter loading?] :as db} dispatch!]
  (let [shown (db/filtered-concepts db)]
    [:main.single.concepts
     [:h1 "Concepts"]
     [:p.muted "The ideas the knowledge base is about, across sources. Each passage's keywords name them."]
     [:input.filter {:type "search" :placeholder "Filter by name or gloss…" :value concept-filter
                     :autocomplete "off"
                     :on {:input #(dispatch! {:db (fn [d] (db/set-concept-filter d (.. % -target -value)))})}}]
     (cond
       (seq shown)
       (list
        [:p.meta {:replicant/key "n"} (count shown) " of " (count concept-index)]
        [:ul.concept-list {:replicant/key "list"}
         (for [{:concept/keys [id label gloss mentions] :as c} shown]
           [:li {:replicant/key id}
            (concept-link dispatch! c {:class "concept-name"} label)
            [:span.count mentions]
            (when gloss [:span.gloss gloss])])])
       (and loading? (nil? concept-index)) [:p.muted "Loading…"]
       (seq concept-index) [:p.muted "No concept matches."]
       :else [:p.muted "No concepts yet. Resolve keywords from the REPL (see dev/user.clj)."])]))

(defn app-view [db dispatch!]
  [:div.app
   (header db dispatch!)
   (error-banner db dispatch!)
   (selection-popover db dispatch!)
   (case (:view db)
     :node    (node-view db dispatch!)
     :keyword (keyword-view db dispatch!)
     :ask      (ask-view db dispatch!)
     :concept  (concept-view db dispatch!)
     :concepts (concepts-view db dispatch!)
     :study    (study-view db dispatch!)
     (home-view db dispatch!))])

^:rct/test
(comment
  (answer-parts "A [1]. B [2][10] [x] end")
  ;=> [[:text "A "] [:cite 1] [:text ". B "] [:cite 2] [:cite 10] [:text " "] [:text "["] [:text "x] end"]]
  )
