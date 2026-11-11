(ns robertluo.fly-learner.ui.core.views.concept
  "A concept's page (what to learn first, book exercises, ideas, mentions),
  a cluster's page (a topic to learn) and the concept index."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.node :as node-ui]
            [robertluo.fly-learner.ui.core.views.quiz :as quiz-ui]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(defn- learn-button
  "Start or stop learning cluster `c` (the header lists the ones being learned)."
  [dispatch! {:cluster/keys [id] :keys [learning?]}]
  [:button.btn {:class (when-not learning? "primary")
                :title (if learning? "Stop learning this topic" "Study this topic now: what to study next comes from it")
                :on {:click #(dispatch! {:db (fn [d] (db/learning-toggled d id (not learning?)))
                                         :pull [(pull/goal id (not learning?))]})}}
   (if learning? "✓ Learning · stop" "Learn this topic")])

(defn- provenance [by]
  (cond (= "human" by) "Curated"
        by             (str "Drafted by " by)))

(def known-at
  "A score from which a prerequisite counts as known on a concept page."
  0.6)

(defn- background-tag []
  [:span.tag.hidden {:title "No passage teaches it: background the sources take for granted"} "background"])

(defn- learn-first
  "What to learn before the concept: the prerequisites not known yet, each
  after what it needs (direct ones with why), the known ones folded; and
  what the concept leads to."
  [db dispatch! {:concept/keys [label needs before needed-by needed-by-count]}]
  (let [why     (into {} (map (juxt :concept/id :why)) needs)
        score   #(db/item-score db (db/concept-item (:concept/id %)))
        [known todo] ((juxt filter remove) #(<= known-at (score %)) before)]
    [:section.learn-first
     [:h2 "Learn first"]
     (cond
       (empty? before) [:p.muted "Nothing: " label " builds on no other concept here. You can start with it."]
       (empty? todo)   [:p.muted "You know everything " label " builds on."]
       :else
       (list
        [:p.hint.muted {:replicant/key "h"}
         (count todo) (if (= 1 (count todo)) " concept" " concepts") " to know before " label
         ", foundations first" (when (seq needs) "; marked → when it builds on one directly") "."]
        (let [first-c (first todo)]
          [:div.start {:replicant/key "s"}
           [:button.btn.primary {:on {:click #(dispatch! (nav/quiz-effects (db/concept-item (:concept/id first-c))))}}
            "Start with " (:concept/label first-c)]])
        [:ol.prereq-path {:replicant/key "p"}
         (for [{:concept/keys [id origin] :as c} todo]
           [:li {:replicant/key (str id) :class (when (why id) "direct")}
            [:span.direct-mark (when (why id) {:title "Built on directly"}) (when (why id) "→")]
            (nav/concept-chip dispatch! c)
            (when (= :inferred origin) (background-tag))
            (t/score-bar (score c))
            (when-let [w (why id)] [:span.why.muted w])])]))
     (when (seq known)
       [:p.known-prereqs [:span.muted "Already known: "]
        (for [c known] (nav/concept-chip dispatch! c))])
     (when (seq needed-by)
       [:div.leads-to
        [:h3 "Leads to"]
        [:div.chips (for [c needed-by] (nav/concept-chip dispatch! c))
         (when (> needed-by-count (count needed-by))
           [:span.muted (str "and " (- needed-by-count (count needed-by)) " more")])]])]))

(defn- book-exercises
  "The exercises the book's author wrote testing the concept, each with the
  learner's best grade so far and a way to try it."
  [db dispatch! id exercises]
  (when (seq exercises)
    [:section.exercises
     [:h2 "Exercises from the book"]
     [:p.hint.muted "Written by the author and graded against their solution: each counts double."]
     [:ul.exercise-list
      (for [{:exercise/keys [label summary page] xid :exercise/id nid :node/id} exercises
            :let [g (db/exercise-grade db xid)]]
        [:li {:replicant/key (str xid) :class (when g "tried")}
         [:span.label (if nid
                        (nav/node-link dispatch! {:node/id nid} {:title (str "Read it on p. " page)} (str "Exercise " label))
                        (str "Exercise " label))]
         [:span.summary summary]
         (if g
           [:span.best {:title "Your best answer so far"} (t/percent g)]
           [:span.best.muted "p. " page])
         [:button.btn {:class (when-not g "primary")
                       :on {:click #(dispatch! (nav/quiz-effects (db/concept-item id) xid))}}
          (if g "Again" "Try it")]])]]))

(defn concept-article
  [db {:concept/keys [id label gloss aliases by mentions origin exercises] :as concept} score dispatch!]
  [:article.concept
   [:nav.crumbs (nav/link dispatch! nav/map-effects "/" {} "Map") " › "
    (when-let [c (db/concept-cluster db id)] (list (nav/cluster-link dispatch! c {:replicant/key "c"}) " ›"))]
   [:span.meta "Concept · " (if (= :inferred origin)
                                "background the sources assume, no passage teaches it"
                                (str (count mentions) (if (= 1 (count mentions)) " passage" " passages")))
    (when-let [p (provenance by)] (str " · " p))]
   [:h1 label]
   (when gloss [:p.summary gloss])
   (when (seq aliases) [:p.aliases [:span.muted "Also written "] (str/join ", " aliases)])
   [:div.known
    [:span.muted "You know this"] (t/score-bar score) [:span.pct (t/percent score)]
    [:button.btn.primary {:on {:click #(dispatch! (nav/quiz-effects (db/concept-item id)))}} "Quiz me on this"]]
   (learn-first db dispatch! concept)
   (book-exercises db dispatch! id exercises)
   (ideas-ui/ideas-panel db dispatch! (db/concept-anchor id) (db/ideas-about-concept db id))
   (ideas-ui/in-your-notes db dispatch! [:concept id] (db/ideas-about-concept db id))
   (when (seq mentions)
     [:section
      [:h2 "Mentioned in"]
      (node-ui/hit-cards dispatch! mentions)])
   [:div.curate
    [:button.btn.quiet {:title "Not a concept: remove it; its names are ignored from now on, so no ingest brings it back"
                        :on {:click #(when #?(:cljs (js/confirm (str "Remove the concept \"" label "\"? Its names will be ignored from now on."))
                                              :clj true)
                                       (dispatch! {:pull [(pull/concept-delete id)]}))}}
     "Remove this concept"]]])

(defn cluster-article [db {:cluster/keys [id label gloss] :keys [concepts source score] :as c} dispatch!]
  (let [ix     (get-in db [:graph :ix])
        detail (get-in db [:clusters id])]
    [:article.cluster
     [:nav.crumbs (nav/link dispatch! nav/map-effects "/" {} "Map") " ›"]
     [:span.meta "Topic · " (count concepts) " concepts"
      (when source (str " · " (:source/title source)))]
     [:h1 label]
     (when gloss [:p.summary gloss])
     [:div.known
      [:span.muted "You know this"] (t/score-bar score) [:span.pct (t/percent score)]
      (learn-button dispatch! c)]
     (when-let [next (seq (:next detail))]
       [:section
        [:h2 "Study next in this topic"]
        (quiz-ui/next-cards dispatch! next)])
     [:section
      [:h2 "Concepts"]
      (if ix
        [:div.chips
         (for [cid concepts :let [cc (get-in ix [:concepts cid])] :when cc]
           (nav/concept-chip dispatch! cc))]
        (t/skeleton 2))]
     [:div.curate
      [:button.btn.quiet {:title "Not knowledge (an edition's colophon, say): remove the topic and its concepts; their names are ignored from now on"
                          :on {:click #(when #?(:cljs (js/confirm (str "Remove the topic \"" label "\" and its " (count concepts)
                                                                       " concepts? Their names will be ignored from now on."))
                                                :clj true)
                                         (dispatch! {:pull [(pull/topic-delete id)]}))}}
       "Remove this topic"]]]))

(defn- add-concept-form
  "Add a concept the model missed: a name, other names, a gloss (written
  from the passages naming it when left empty)."
  [{{:keys [label aliases gloss adding?]} :concept-form :as db} dispatch!]
  (let [typed (fn [k] {:input #(dispatch! {:db (fn [d] (db/concept-form-typed d k (.. % -target -value)))})})]
    [:details.add-concept {:open (or adding? (not (str/blank? label)))}
     [:summary "Add a concept the model missed"]
     [:form {:on {:submit (fn [e]
                            (.preventDefault e)
                            (when (db/concept-form-value db)
                              (dispatch! {:db db/concept-adding :pull :concept-create})))}}
      [:label "Name" [:input {:type "text" :value label :disabled adding? :placeholder "e.g. tail recursion"
                              :required true :maxlength 80 :on (typed :label)}]]
      [:label "Other names" [:input {:type "text" :value aliases :disabled adding?
                                     :placeholder "comma-separated, e.g. tail call, iterative process"
                                     :on (typed :aliases)}]]
      [:label "Gloss" [:textarea {:rows 2 :value gloss :disabled adding? :maxlength 500
                                  :placeholder "One sentence fixing its sense (optional: written from the passages naming it)"
                                  :on (typed :gloss)}]]
      [:div.actions
       [:button.btn.primary {:type "submit" :disabled (or adding? (str/blank? label))}
        (if adding? "Adding…" "Add")]
       [:span.muted (if adding?
                      "Finding the passages that name it, its topic and what it needs first…"
                      "It is found in every passage that names it (whole words, any case).")]]]]))

(defn term-view
  "A term of the learner's: a `[[..]]` link of their notes naming no concept.
  What their notes say of it, the passages of the sources naming it, and
  making it a concept of the knowledge base (their choice: until then it is
  theirs only)."
  [{:keys [term named loading?] :as db} dispatch!]
  (let [{:keys [label notes]} (get (db/my-terms db) term)
        hits   (get named term)
        adding? (get-in db [:concept-form :adding?])]
    [:main.single.term
     [:span.meta "Yours · " (count notes) (if (= 1 (count notes)) " note names it" " notes name it")]
     [:h1 (or label term)]
     [:p.muted "A term of your notes that names no concept of the sources. It is yours: it stays beside the sources' knowledge and never moves a score."]
     (or (ideas-ui/in-your-notes db dispatch! [:term term] nil)
         [:p.muted "None of your notes names it now."])
     [:section
      [:h2 "Named in the sources"]
      (cond
        (seq hits)
        (list [:p.muted {:replicant/key "p"} "Passages whose text names it."]
              [:div.curate {:replicant/key "add"}
               [:button.btn {:disabled adding?
                             :title "Add it to the knowledge base as a concept the model missed: tagged on these passages, given a gloss, placed in a topic"
                             :on {:click #(dispatch! {:db (fn [d] (-> d
                                                                      (assoc :concept-form {:label (or label term) :aliases "" :gloss ""})
                                                                      db/concept-adding))
                                                      :pull :concept-create})}}
                (if adding? "Adding…" "Make it a concept")]]
              (with-meta (node-ui/hit-cards dispatch! hits) {:key "hits"}))
        loading? (t/skeleton)
        :else    [:p.muted "No passage names it: it is new knowledge, yours."])]]))

(defn concepts-view [{:keys [concept-index concept-filter loading?] :as db} dispatch!]
  (let [shown (db/filtered-concepts db)]
    [:main.single.concepts
     [:h1 "Concepts"]
     [:p.muted "The ideas the knowledge base is about, across sources. Each passage's keywords name them."]
     (add-concept-form db dispatch!)
     (when-let [ts (seq (sort-by :term (vals (db/my-terms db))))]
       [:p.your-terms [:span.muted "Your terms "]
        (for [{:keys [term label]} ts]
          (nav/term-link dispatch! term {:class "chip yours" :replicant/key term} label))])
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
            (nav/concept-link dispatch! c {:class "concept-name"} label)
            [:span.count mentions]
            (when gloss [:span.gloss gloss])])])
       (and loading? (nil? concept-index)) (t/skeleton 8)
       (seq concept-index) [:p.muted "No concept matches."]
       :else [:p.muted "No concepts yet. Resolve keywords from the REPL (see dev/user.clj)."])]))
