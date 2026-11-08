(ns robertluo.fly-learner.ui.core.views.nav
  "Links and effect maps to the app's places: a node, a concept, a
  cluster, an answer, an exploration, the map; and starting a quiz or a
  question. Each link has a real href (open in a new tab works) and is
  followed in-app on a plain click."
  (:require [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.history :as history]))

(defn link
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

(defn node-link
  ([dispatch! node] (node-link dispatch! node {} (:node/title node)))
  ([dispatch! {:node/keys [id]} attrs & children]
   (apply link dispatch! (open-node-effects id) (str "/n/" id) attrs children)))

(defn keyword-link [dispatch! kw]
  (link dispatch! {:db #(db/open-keyword % kw) :pull :keyword :history :push}
        (history/state->path {:view :keyword :keyword kw})
        {:class ["chip" "keyword"] :title "Keyword (names no concept)"}
        kw))

(defn concept-effects [id]
  {:db #(db/open-concept % id) :pull [:concept :concept-graph :learning :ideas] :history :push :animate true})

(defn concept-link
  "Link to a concept page; the gloss shows on hover."
  [dispatch! {:concept/keys [id label gloss]} attrs & children]
  (apply link dispatch! (concept-effects id)
         (history/state->path {:view :concept :concept-id id})
         (merge {:title (or gloss label)} attrs)
         (or (seq children) [label])))

(defn concept-chip [dispatch! c & extra]
  (apply concept-link dispatch! c {:class ["chip" "concept"] :replicant/key (:concept/id c)}
         (:concept/label c) extra))

(def map-effects {:db db/go-home :pull [:universe :learning :ideas] :history :push})

(defn cluster-effects [id]
  {:db #(db/open-cluster % id) :pull [:universe :cluster :concept-graph :learning :ideas]
   :history :push :animate true})

(defn cluster-link
  "Link to a cluster's page; its gloss shows on hover."
  [dispatch! {:cluster/keys [id label gloss]} attrs & children]
  (apply link dispatch! (cluster-effects id)
         (history/state->path {:view :cluster :cluster-id id})
         (merge {:title (or gloss label)} attrs)
         (or (seq children) [label])))

(defn quiz-effects
  "Quiz on `item` (nil: what to study next), where the learner is; on its
  book exercise `exercise` (an id) when given."
  ([item] (quiz-effects item nil))
  ([item exercise]
   {:db #(db/quiz-start % item exercise) :pull [:question-create :learning]}))

(defn ask-effects
  "Ask `q`: show it, search at once, answer with the model."
  [q]
  {:db #(-> % db/box-cleared (db/ask-submitted q)) :pull [:search :ask-create]})

(defn ask-link [dispatch! {:ask/keys [id]} attrs & children]
  (apply link dispatch! {:db #(db/open-ask % id) :pull :ask :history :push} (str "/a/" id) attrs children))

(defn explore-link [dispatch! id attrs & children]
  (apply link dispatch! {:db #(db/open-explore % id) :pull [:exploration :explorations] :history :push}
         (history/state->path {:view :explore :exploration-id id}) attrs children))

(defn bookmark-link
  "Link to go on reading at bookmark `b` (see db/bookmarks)."
  [dispatch! {:keys [node-id page] :as b} attrs & children]
  (apply link dispatch! {:db #(db/open-bookmark % b) :pull [:node :ideas] :history :push}
         (history/state->path {:view :node :node-id node-id :pdf {:page page}})
         attrs children))
