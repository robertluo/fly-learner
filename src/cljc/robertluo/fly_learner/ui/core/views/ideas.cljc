(ns robertluo.fly-learner.ui.core.views.ideas
  "The learner's own voice (robertluo.fly-learner.ideas): reactions, notes,
  quotes marked in text, and bookmarks."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ideas :as ideas]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(def stance-mark {:agree "✓" :disagree "✗" :note "✎" :highlight "▍" :bookmark "🔖"})

(def stance-label {:agree "Agree" :disagree "Disagree" :note "Note" :highlight "Highlight" :bookmark "Bookmark"})

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

(defn composer
  "The note being written, with ideas it may link to."
  [db dispatch!]
  (let [{:keys [anchors text links]} (get-in db [:ideas :draft])
        others (->> (get-in db [:ideas :all]) (filter :idea/text) (take-last 10) reverse)]
    [:div.composer
     [:textarea {:value text :rows (if (> (count text) 200) 9 3) :placeholder "Your idea, in your own words…"
                 :on {:input #(dispatch! {:db (fn [d] (db/draft-typed d (.. % -target -value)))})}}]
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
                            :on {:click #(dispatch! {:pull (pull/idea-create {:idea/stance :note :idea/text text
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
                                                             (update :pull conj (pull/idea-delete (:idea/id mine))))
                                                    (not on?) (update :pull conj (pull/idea-create {:idea/stance stance
                                                                                                  :idea/anchors [anchor]}))))}}
       (stance-mark stance) " " (stance-label stance)])))

(defn- idea-item [db dispatch! {:idea/keys [id stance text anchors links targets]} show-target?]
  (let [by-id (into {} (map (juxt :idea/id identity)) (get-in db [:ideas :all]))]
    [:li.idea {:replicant/key (str id) :class (name stance)}
     [:span.stance {:title (stance-label stance)} (stance-mark stance)]
     [:div.idea-body
      (for [[i a] (map-indexed vector anchors) :when (:quote a)]
        [:blockquote {:replicant/key i} (t/snippet (:quote a) 300)])
      (if text [:p text] [:p.muted (stance-label stance)])
      (when show-target?
        [:span.meta "On " (str/join ", " (map :title targets))])
      (when (seq links)
        [:span.links "↔ " (str/join " · " (keep #(t/snippet (:idea/text (by-id %)) 50) links))])]
     [:button.btn.quiet.remove {:title "Remove" :on {:click #(dispatch! {:db (fn [d] (db/idea-removed d id))
                                                                         :pull (pull/idea-delete id)})}}
      "×"]]))

(defn ideas-panel
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
         [:p.hint.muted "Agree, disagree or add a note. Select any sentence to highlight it or react to just that."]))]))

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
