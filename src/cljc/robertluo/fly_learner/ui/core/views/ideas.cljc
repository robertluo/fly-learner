(ns robertluo.fly-learner.ui.core.views.ideas
  "The learner's own voice (robertluo.fly-learner.ideas): reactions, notes
  (Markdown, one stance each: a note, agreeing, restating, disagreeing;
  `[[a term]]` links the concept it names), quotes marked in text, and
  bookmarks."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views.markdown :as md]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(def stance-mark {:agree "✓" :restate "≈" :disagree "✗" :note "✎" :highlight "▍" :bookmark "🔖"})

(def stance-label {:agree "Agree" :restate "Restate" :disagree "Disagree" :note "Note" :highlight "Highlight"
                   :bookmark "Bookmark"})

(def ^:private stance-prompt
  {:note     "Your idea, in your own words…"
   :agree    "Why you agree, what it adds up to…"
   :restate  "Say it again, in your own words…"
   :disagree "Why you disagree, what you would say instead…"})

(defn written
  "What the learner wrote, Markdown: a `[[term]]` link opens the concept it
  names (`refs` {term concept}), or the learner's own term; nil refs (not
  known yet: a preview) links nothing."
  [dispatch! text refs]
  (md/rich text {:wikilink (fn [term shown]
                             (cond
                               (nil? refs)        [:span.wikilink shown]
                               (get refs term)    (nav/concept-link dispatch! (get refs term) {:class "wikilink"} shown)
                               :else              (nav/term-link dispatch! term {:class "wikilink yours"} shown)))}))

(defn ref-link
  "A link to what a term in the learner's notes names (db/note-ref): a
  concept, or their own term."
  [dispatch! {:keys [concept term label]}]
  (if concept
    (nav/concept-link dispatch! concept {:class "chip concept"})
    (nav/term-link dispatch! term {:class "chip yours"} label)))

(defn anchored-text
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

(defn- pick-wiki!
  "The `[[` link being typed becomes `[[label]]`; the caret goes after it."
  [db dispatch! label]
  (let [[_ caret] (db/wiki-picked db label)]
    (dispatch! {:db #(first (db/wiki-picked % label))})
    #?(:cljs (js/requestAnimationFrame
              #(when-let [^js ta (js/document.querySelector ".composer textarea")]
                 (.focus ta)
                 (when caret (.setSelectionRange ta caret caret))))
       :clj caret)))

(defn- wiki-keys
  "Tab takes the first suggestion, Escape closes them."
  [db dispatch! suggestions]
  (fn [e]
    (when (seq suggestions)
      (case (.-key e)
        "Tab"    (do (.preventDefault e) (pick-wiki! db dispatch! (:concept/label (first suggestions))))
        "Escape" (do (.preventDefault e) (.stopPropagation e) (dispatch! {:db db/draft-wiki-closed}))
        nil))))

(defn composer
  "The idea being written: its stance, its text in Markdown (written or
  previewed; `[[` suggests concepts), ideas it may link to."
  [db dispatch!]
  (let [{:keys [anchors stance text links preview? editing]} (get-in db [:ideas :draft])
        stance (or stance :note)
        suggestions (when-not preview? (take 6 (db/wiki-suggestions db)))
        others (->> (get-in db [:ideas :all]) (filter :idea/text) (remove #(= editing (:idea/id %)))
                    (take-last 10) reverse)]
    [:div.composer {:class (name stance)}
     [:div.composer-bar
      [:div.stance-picker {:role "radiogroup" :aria-label "Stance"}
       (for [st db/draft-stances]
         [:button.chip {:replicant/key (name st) :class [(name st) (when (= st stance) "on")]
                        :role "radio" :aria-checked (str (= st stance))
                        :on {:click #(dispatch! {:db (fn [d] (db/draft-stance d st))})}}
          (stance-mark st) " " (stance-label st)])]
      [:div.write-preview
       [:button.btn.quiet {:class (when-not preview? "on") :on {:click #(dispatch! {:db (fn [d] (db/draft-preview d false))})}} "Write"]
       [:button.btn.quiet {:class (when preview? "on") :disabled (str/blank? text)
                           :on {:click #(dispatch! {:db (fn [d] (db/draft-preview d true))})}} "Preview"]]]
     (if preview?
       [:div.preview (written dispatch! text nil)]
       [:textarea {:value text :rows (if (> (count text) 200) 9 4) :placeholder (stance-prompt stance)
                   :on {:input   #(let [v (.. % -target -value) caret (.. % -target -selectionStart)]
                                    (dispatch! {:db (fn [d] (db/draft-typed d v caret)) :pull :wiki}))
                        :keydown (wiki-keys db dispatch! suggestions)}}])
     (if (seq suggestions)
       [:ul.wiki-suggest {:role "listbox"}
        (for [[i {:concept/keys [id label gloss]}] (map-indexed vector suggestions)]
          [:li {:replicant/key (str id)}
           [:button {:type "button" :class (when (zero? i) "first") :title gloss
                     :on {:mousedown #(.preventDefault %) ; keep the caret in the text
                          :click #(pick-wiki! db dispatch! label)}}
            [:span.label label] (when gloss [:span.gloss (t/snippet gloss 70)])]])]
       [:p.composer-hint.muted "Markdown; formulas in " [:code "$..$"] "; " [:code "[[a concept]]"] " links it, "
        [:code "[[A]] needs [[B]]"] " says what comes first."])
     (when (seq others)
       [:div.link-picker
        [:span.muted "Links to "]
        (for [{:idea/keys [id stance text]} others]
          [:button.chip {:replicant/key (str id)
                         :class (when (contains? links id) "on")
                         :on {:click #(dispatch! {:db (fn [d] (db/draft-link d id))})}}
           (stance-mark stance) " " (t/snippet text 40)])])
     [:div.actions
      [:button.btn.primary {:disabled (str/blank? text)
                            :on {:click #(dispatch! {:pull (if editing
                                                             (pull/idea-update editing {:idea/stance stance :idea/text text
                                                                                        :idea/links (vec links)})
                                                             (pull/idea-create {:idea/stance stance :idea/text text
                                                                                :idea/anchors anchors
                                                                                :idea/links (vec links)}))})}}
       "Save"]
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
                                                             (update :pull conj (pull/idea-delete (:idea/id mine))))
                                                    (not on?) (update :pull conj (pull/idea-create {:idea/stance stance
                                                                                                  :idea/anchors [anchor]}))))}}
       (stance-mark stance) " " (stance-label stance)])))

(defn edit-button
  "Rewrite idea `i` (what the learner wrote) in place."
  [dispatch! i]
  (when (and (:idea/text i) (some #{(:idea/stance i)} db/draft-stances))
    [:button.btn.quiet.edit {:title "Edit" :on {:click #(dispatch! {:db (fn [d] (db/draft-edit d i))})}} "Edit"]))

(defn editing? [db id] (= id (get-in db [:ideas :draft :editing])))

(defn- idea-item [db dispatch! {:idea/keys [id stance text anchors links targets refs] :as idea} show-target?]
  (let [by-id (into {} (map (juxt :idea/id identity)) (get-in db [:ideas :all]))]
    (if (editing? db id)
      [:li.idea.editing {:replicant/key (str id) :class (name stance)} (composer db dispatch!)]
    [:li.idea {:replicant/key (str id) :class (name stance)}
     [:span.stance {:title (stance-label stance)} (stance-mark stance)]
     [:div.idea-body
      (for [[i a] (map-indexed vector anchors) :when (:quote a)]
        [:blockquote {:replicant/key i} (t/snippet (:quote a) 300)])
      (if text (written dispatch! text refs) [:p.muted (stance-label stance)])
      (when show-target?
        [:span.meta "On " (str/join ", " (map :title targets))])
      (when (seq links)
        [:span.links "↔ " (str/join " · " (keep #(t/snippet (:idea/text (by-id %)) 50) links))])]
     [:div.idea-actions
      (edit-button dispatch! idea)
      [:button.btn.quiet.remove {:title "Remove" :on {:click #(dispatch! {:db (fn [d] (db/idea-removed d id))
                                                                          :pull (pull/idea-delete id)})}}
       "×"]]])))

(defn in-your-notes
  "What the learner's notes say of `k` ([:concept id] or [:term key]): the
  prerequisites they state, the notes naming it (but those in `shown`).
  Theirs, beside the sources' knowledge: nothing here moves a score or
  orders what to study."
  [db dispatch! k shown]
  (let [{:keys [needs needed-by notes]} (db/in-my-notes db k)
        shown (set (map :idea/id shown))
        notes (remove (comp shown :idea/id) notes)]
    (when (or (seq needs) (seq needed-by) (seq notes))
      [:section.in-notes
       [:h2 "In your notes"]
       (when (seq needs)
         [:p.claims [:span.muted "You say it needs "] (for [r needs] (ref-link dispatch! r))])
       (when (seq needed-by)
         [:p.claims [:span.muted "You say it comes before "] (for [r needed-by] (ref-link dispatch! r))])
       (when (seq notes)
         [:ul.idea-list (for [i (reverse notes)] (idea-item db dispatch! i true))])])))

(defn ideas-panel
  "Your ideas on `anchor` (and on `ideas` about things inside it)."
  [db dispatch! anchor ideas]
  (let [draft (get-in db [:ideas :draft])]
    [:section.ideas
     [:h2 "Your ideas"]
     [:div.reactions
      (reaction-buttons db dispatch! anchor)
      [:button.btn {:on {:click #(dispatch! {:db (fn [d] (db/draft-start d [anchor] false))})}} "✎ Write"]]
     (when (and draft (not (:from-selection? draft)) (not (:editing draft)) (= [anchor] (:anchors draft)))
       (composer db dispatch!))
     (let [shown (remove #(and (nil? (:idea/text %)) (= [anchor] (:idea/anchors %))) ideas)]
       (if (seq shown)
         [:ul.idea-list (for [i (reverse shown)]
                          (idea-item db dispatch! i (not= [anchor] (:idea/anchors i))))]
         [:p.hint.muted "Agree, disagree, or write: a note, why you agree or disagree, or the idea in your own words. Select any sentence to highlight it or react to just that."]))]))

(defn- bookmark-icon [on?]
  [:svg.icon {:viewBox "0 0 16 16" :width 16 :height 16 :aria-hidden "true"}
   [:path {:d "M4 2.5h8v11l-4-3-4 3z" :fill (if on? "currentColor" "none") :stroke "currentColor"
           :stroke-width 1.5 :stroke-linejoin "round"}]])

(defn bookmark-toggle
  "Bookmark the passage open, at the page shown; or take the bookmark away."
  [db node dispatch!]
  (let [b (db/bookmark-on db (:node/id node))]
    [:button.bookmark {:class (when b "on")
                       :title (if b
                                (str "Remove the bookmark (made at p. " (:page b) ")")
                                (str "Bookmark this place: " (:node/title node) ", p. " (db/doc-page db)))
                       :aria-pressed (str (boolean b))
                       :on {:click #(dispatch! (if b
                                                 {:db (fn [d] (db/idea-removed d (:idea/id b)))
                                                  :pull (pull/idea-delete (:idea/id b))}
                                                 {:pull (pull/idea-create {:idea/stance :bookmark
                                                                         :idea/anchors [(db/bookmark-anchor db node)]})}))}}
     (bookmark-icon (some? b))]))

(defn bookmark-list
  "Bookmarks to go on reading from: the passage, where, when."
  [dispatch! bms show-source?]
  [:ul.bookmarks
   (for [{:keys [page] :as b} bms
         :let [source-title (:source/title b)]]
     [:li {:replicant/key (str (:idea/id b))}
      (nav/bookmark-link dispatch! b {:class "bookmark-link" :title "Go on reading here"}
                         (bookmark-icon true) [:span.bm-title (:title b)])
      [:span.meta (when show-source? (str source-title " · ")) (when page (str "p. " page))]])])
