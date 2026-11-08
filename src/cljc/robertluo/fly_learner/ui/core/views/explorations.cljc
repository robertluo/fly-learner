(ns robertluo.fly-learner.ui.core.views.explorations
  "Explorations: one kept (its interactive view, sources, remaking it) and
  the list with the hand-written examples."
  (:require [robertluo.fly-learner.explore :as explore]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.explore :as explore-view]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.node :as node-ui]))

(defn- exploration-cite [dispatch! cites]
  (fn [n]
    (if-let [node (get cites (dec n))]
      (nav/node-link dispatch! node {:class "cite" :title (:node/title node)} (str n))
      [:span.cite.gone {:title "Not in the knowledge base"} (str n)])))

(defn- kept-exploration [{:keys [exploring explore-feedback] :as db} {:exploration/keys [id node cites feedback] :as x} dispatch!]
  (let [spec (:exploration/spec x)]
    [:main.single.explore
     [:nav.crumbs (nav/explore-link dispatch! nil {} "Explorations") " › "
      (when node [:span (nav/node-link dispatch! node) " › "])]
     [:span.meta "Exploration"
      (when-let [prev (:exploration/previous x)]
        [:span " · remade from " (nav/explore-link dispatch! prev {} "an earlier version")
         (when feedback (str ": “" feedback "”"))])]
     [:h1 (:title spec)]
     (explore-view/exploration spec (db/exploration-state db id)
                               (fn [action] (dispatch! {:db #(db/explored % id action)}))
                               {:cite (exploration-cite dispatch! cites)})
     [:section.x-sources
      [:h2 "Sources"]
      (let [nums (filter #(get cites (dec %)) (explore/cited-numbers spec))]
        (when (seq nums)
          [:ol
           (for [i nums :let [n (get cites (dec i))]]
             [:li {:replicant/key i :value i} (nav/node-link dispatch! n) (node-ui/meta-line n)])]))
      [:p.hint.muted "Made by a model from " (if node (nav/node-link dispatch! node) "a passage")
       " and checked by running it, not for truth: compare it with the text."]]
     [:section.x-remake
      [:h2 "Not right?"]
      (if (= id (:previous exploring))
        (node-ui/making-note "Making it again.")
        [:form {:on {:submit (fn [e] (.preventDefault e)
                               (dispatch! {:db #(db/explore-remake % id) :pull :exploration-create}))}}
         [:textarea {:rows 2 :placeholder "What to change, e.g. \"use the example on p. 12\", \"a 3×3 game\" (optional)"
                     :value explore-feedback
                     :on {:input #(dispatch! {:db (fn [d] (db/explore-feedback d (.. % -target -value)))})}}]
         [:div.actions
          [:button.btn.primary {:type "submit" :disabled (some? exploring)} "Make it again"]
          [:button.btn.quiet {:type "button"
                              :on {:click #(when #?(:cljs (js/confirm "Delete this exploration?") :clj true)
                                             (dispatch! {:pull :exploration-delete}))}}
           "Delete"]]])]]))

(defn explore-view [{:keys [exploration-id exploration-list loading?] :as db} dispatch!]
  (cond
    (db/current-exploration db)
    (kept-exploration db (db/current-exploration db) dispatch!)

    (and (string? exploration-id) (db/exploration-spec db exploration-id))
    (let [spec (db/exploration-spec db exploration-id)]
      [:main.single.explore
       [:nav.crumbs (nav/explore-link dispatch! nil {} "Explorations") " › "]
       [:span.meta "Example, written by hand"]
       [:h1 (:title spec)]
       (explore-view/exploration spec (db/exploration-state db exploration-id)
                                 (fn [action] (dispatch! {:db #(db/explored % exploration-id action)})))])

    (uuid? exploration-id)
    [:main.single.explore (when loading? [:p.muted "Loading…"])]

    :else
    [:main.single.explore
     [:span.meta "Explorations"]
     [:h1 "Try it out"]
     [:p.muted "Passages that are better understood by doing offer “Explore it” on their page: "
      "a model builds a small interactive page from the passage, checked by running it, and it is kept here."]
     (when exploration-id [:p.muted "No exploration " (pr-str exploration-id) "."])
     (when (seq exploration-list)
       [:ul.cards.compact
        (for [{:exploration/keys [id title node pitch]} exploration-list]
          [:li.card {:replicant/key (str id)}
           (nav/explore-link dispatch! id {:class "card-title"} title)
           (when node [:span.meta (:node/title node) (when-let [p (:node/page node)] (str " · p. " p))])
           (when pitch [:p.preview pitch])])])
     [:h2 "Examples"]
     [:ul.cards.compact
      (for [[id spec] (sort-by (comp :title val) db/example-specs)]
        [:li.card {:replicant/key id}
         (nav/explore-link dispatch! id {:class "card-title"} (:title spec))
         [:p.preview (:intro spec)]])]]))
