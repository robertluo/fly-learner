(ns learner.app
  "Browser UI: browse the folded DAG from birdview down to source
  paragraphs; keywords and cross links are clickable."
  (:require [replicant.dom :as r]))

(defonce state (atom {:sources [] :node nil}))

(defn- fetch-json! [url on-ok]
  (-> (js/fetch url #js {:headers #js {"Accept" "application/json"}})
      (.then #(.json %))
      (.then #(on-ok (js->clj % :keywordize-keys true)))))

(defn open-node! [id]
  (fetch-json! (str "/api/nodes/" id) #(swap! state assoc :node %)))

(defn load-sources! []
  (fetch-json! "/api/sources" #(swap! state assoc :sources %)))

(defn node-link [{:node/keys [id title]}]
  [:a {:href "#" :on {:click [[:open-node id]]}} title])

(defn node-view [{:node/keys [title summary text level children links]}]
  [:article
   [:h2 title [:small " (level " level ")"]]
   (when summary [:p summary])
   (when text [:blockquote text])
   (when (seq children)
     [:section [:h3 "Drill down"]
      [:ul (for [c (sort-by :node/order children)] [:li {:replicant/key (:node/id c)} (node-link c)])]])
   (when (seq links)
     [:section [:h3 "Related"]
      [:ul (for [l links] [:li {:replicant/key (:node/id l)} (node-link l)])]])])

(defn root-view [{:keys [sources node]}]
  [:main
   [:h1 "Learner"]
   [:nav
    [:ul (for [{:source/keys [id title root]} sources]
           [:li {:replicant/key id} (node-link {:node/id (:node/id root) :node/title title})])]]
   (when node (node-view node))])

(defn- dispatch [_ actions]
  (doseq [[action & args] actions]
    (case action
      :open-node (open-node! (first args)))))

(defonce el (js/document.getElementById "app"))

(defn render! [] (r/render el (root-view @state)))

(defn ^:dev/after-load reload [] (render!))

(defn init []
  (r/set-dispatch! (fn [{:replicant/keys [dom-event]} actions]
                     (some-> dom-event .preventDefault)
                     (dispatch nil actions)))
  (add-watch state ::render (fn [_ _ _ _] (render!)))
  (render!)
  (load-sources!))
