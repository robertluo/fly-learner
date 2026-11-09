(ns robertluo.fly-learner.ui.core.views.notebook
  "The notebook: what the learner wrote while reading, as a book of their
  own. As the sources go (a source, its chapters, each note under the
  passage it is about, the quote it was written on) or newest first;
  highlights too if asked. Each note can be rewritten in place, and the
  whole downloaded as Markdown (db/notebook-markdown: `[[..]]` links kept,
  so it opens as notes elsewhere)."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.nav :as nav]))

(defn- where
  "Where an entry is: its passage (page and section), or what it is about."
  [dispatch! {:idea/keys [targets] :as i}]
  (if-let [{:keys [page title] nid :node/id} (db/entry-where i)]
    (let [label (str/join " · " (remove nil? [(when page (str "p. " page)) title]))]
      (if nid
        (nav/node-link dispatch! {:node/id nid} {:class "where" :title "Open the passage"} label)
        [:span.where label]))
    [:span.where "On "
     (interpose ", " (for [{:keys [key title]} targets]
                       (if (= :concept (first key))
                         (nav/concept-link dispatch! {:concept/id (second key) :concept/label title}
                                           {:replicant/key (str key)})
                         [:span {:replicant/key (str key)} title])))]))

(defn- entry [db dispatch! {:idea/keys [id stance text anchors refs] :as i}]
  (let [q (some :quote anchors)]
    [:div.entry {:replicant/key (str id) :class (name stance)}
     (when q [:blockquote (str q)])
     [:div.entry-head
      (where dispatch! i)
      [:span.entry-actions
       (when-not (ideas-ui/editing? db id) (ideas-ui/edit-button dispatch! i))
       [:button.btn.quiet.remove {:title "Remove"
                                  :on {:click #(when #?(:cljs (js/confirm "Remove this from your notebook?") :clj true)
                                                 (dispatch! {:db (fn [d] (db/idea-removed d id))
                                                             :pull (pull/idea-delete id)}))}}
        "×"]]]
     (cond
       (ideas-ui/editing? db id) (ideas-ui/composer db dispatch!)
       text [:div.entry-text
             [:span.stance {:title (ideas-ui/stance-label stance)} (ideas-ui/stance-mark stance)]
             (ideas-ui/written dispatch! text refs)])]))

(defn- download!
  "Save the notebook as a Markdown file."
  #?(:cljs [db] :clj [_db])
  #?(:cljs (let [blob (js/Blob. #js [(db/notebook-markdown db)] #js {:type "text/markdown"})
                 url  (js/URL.createObjectURL blob)
                 a    (js/document.createElement "a")]
             (set! (.-href a) url)
             (set! (.-download a) "notebook.md")
             (.click a)
             (js/setTimeout #(js/URL.revokeObjectURL url) 1000))
     :clj nil))

(defn notebook-view [db dispatch!]
  (let [parts (db/notebook db)
        {:keys [order highlights?]} (:notebook db)
        n     (count (mapcat :entries (mapcat :chapters parts)))]
    [:main.single.notebook
     [:span.meta "Yours · " n (if (= 1 n) " entry" " entries")]
     [:h1 "Notebook"]
     [:p.muted "What you wrote while reading, as a book of your own. Write on any passage or concept, or on a sentence you select."]
     [:div.notebook-bar
      [:div.write-preview
       (for [[o label] [[:sources "As the sources go"] [:newest "Newest first"]]]
         [:button.btn.quiet {:replicant/key (name o) :class (when (= o order) "on")
                             :on {:click #(dispatch! {:db (fn [d] (db/notebook-order d o))})}}
          label])]
      [:label.toggle
       [:input {:type "checkbox" :checked (boolean highlights?)
                :on {:change #(dispatch! {:db (fn [d] (db/notebook-highlights d (.. % -target -checked)))})}}]
       " Highlights"]
      [:button.btn {:disabled (zero? n) :title "The notebook as one Markdown file" :on {:click #(download! db)}}
       "Download Markdown"]]
     (cond
       (seq parts)
       (for [{:keys [title chapters] sid :source/id} parts]
         [:section.part {:replicant/key (str (or sid title "all"))}
          (when title [:h2 title])
          (for [[k {ct :title es :entries}] (map-indexed vector chapters)]
            [:div.chapter {:replicant/key k}
             (when ct [:h3 ct])
             (for [i es] (entry db dispatch! i))])])

       (nil? (get-in db [:ideas :all])) [:p.muted "Loading…"]
       :else [:p.muted "Nothing yet. Open a passage or a concept and write (✎ Write), or select a sentence and choose ✎ Note."])]))
