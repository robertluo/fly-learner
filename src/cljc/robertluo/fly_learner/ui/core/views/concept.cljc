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
   (when (seq mentions)
     [:section
      [:h2 "Mentioned in"]
      (node-ui/hit-cards dispatch! mentions)])])

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
        [:p.muted "Loading…"])]]))

(defn concepts-view [{:keys [concept-index concept-filter loading?] :as db} dispatch!]
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
            (nav/concept-link dispatch! c {:class "concept-name"} label)
            [:span.count mentions]
            (when gloss [:span.gloss gloss])])])
       (and loading? (nil? concept-index)) [:p.muted "Loading…"]
       (seq concept-index) [:p.muted "No concept matches."]
       :else [:p.muted "No concepts yet. Resolve keywords from the REPL (see dev/user.clj)."])]))
