(ns robertluo.fly-learner.ui.core
  "SPA entry point: the dispatch-of effect pattern (as in flybot.sg).

  dispatch! takes an effect map; effects run in `effect-order`:
    :db      pure state updater (swap! app-db f)
    :pull    named pull op (robertluo.fly-learner.ui.core.pull), {:request .. :then ..},
             or a vector of these (sent in parallel, see robertluo.fly-learner.ui.core.api)
    :history :push or :replace the URL for the new state
    :animate true: play the concept graph's layout change (advances
             [:graph :t] from 0 to 1 over `animation-ms`)"
  (:require [robertluo.fly-learner.ui.core.api :as api]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.history :as history]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views :as views]
            [robertluo.fly-learner.ui.select :as select]
            [replicant.dom :as r]))

;;=============================================================================
;; Dispatch
;;=============================================================================

(defonce app-db (atom db/initial-state))

(def ^:private effect-order [:db :pull :history :animate])

(def ^:private animation-ms 450)

(defonce ^:private animation (atom 0)) ; generation: a newer animation stops older loops

(defn- animate!
  "Advance the graph animation each frame until done or superseded."
  [app-db]
  (let [gen   (swap! animation inc)
        start (js/performance.now)]
    (letfn [(frame [now]
              (when (= gen @animation)
                (let [t (min 1 (/ (- now start) animation-ms))]
                  (swap! app-db db/graph-tick t)
                  (when (< t 1) (js/requestAnimationFrame frame)))))]
      (when (db/graph-animating? @app-db)
        (js/requestAnimationFrame frame)))))

(defn dispatch-of [app-db]
  (let [self (volatile! nil)
        dispatch!
        (fn [effects]
          (doseq [type effect-order
                  :let [effect (get effects type)]
                  :when (some? effect)]
            (case type
              :db      (swap! app-db effect)
              :pull    (doseq [op (if (vector? effect) effect [effect])]
                         (when-let [{:keys [request then]}
                                    (if (map? op) op (pull/resolve-pull op @app-db))]
                           (api/send! request
                                      #(@self (then %))
                                      #(@self {:db (fn [d] (db/set-error d %))}))))
              :history (if (= :replace effect)
                         (history/replace-state! @app-db)
                         (history/push-state! @app-db))
              :animate (animate! app-db))))]
    (vreset! self dispatch!)
    dispatch!))

(defonce dispatch! (dispatch-of app-db))

;;=============================================================================
;; Rendering
;;=============================================================================

(defn ^:export render!
  "Re-render (also the shadow-cljs :after-load hook)."
  []
  (when-let [el (js/document.getElementById "app")]
    (r/render el (views/app-view @app-db dispatch!))))

(def ^:private source-only-key "fly-learner/source-only")
(def ^:private doc-zoom-key "fly-learner/doc-zoom")
(def ^:private split-key "fly-learner/split")
(def ^:private cards-key "fly-learner/cards-all")

(defonce _render-on-change
  (add-watch app-db ::render
             (fn [_ _ old new]
               (render!)
               ;; a reaction was made (or dismissed): let go of the selected text
               (when (and (get-in old [:ideas :selection]) (not (get-in new [:ideas :selection])))
                 (select/clear!))
               (when (not= (:source-only? old) (:source-only? new))
                 (some-> js/window.localStorage (.setItem source-only-key (str (:source-only? new)))))
               (when (not= (get-in old [:cards :all?]) (get-in new [:cards :all?]))
                 (some-> js/window.localStorage (.setItem cards-key (str (get-in new [:cards :all?])))))
               (when (not= (:split old) (:split new))
                 (some-> js/window.localStorage (.setItem split-key (str (:split new)))))
               (when (not= (db/doc-zoom old) (db/doc-zoom new))
                 (some-> js/window.localStorage (.setItem doc-zoom-key (str (db/doc-zoom new)))))
               (when (not= (select-keys old [:view :node-id :ask-id :keyword :concept-id])
                           (select-keys new [:view :node-id :ask-id :keyword :concept-id]))
                 (js/window.scrollTo 0 0)))))

;;=============================================================================
;; Init
;;=============================================================================

(defn- go-to-route! [route]
  (dispatch! {:db #(db/apply-route % route) :pull (pull/for-route route)}))

(defn ^:export init! []
  ;; the left pane stays hidden across visits (a small screen reading the source)
  (when (= "true" (some-> js/window.localStorage (.getItem source-only-key)))
    (dispatch! {:db #(db/source-only % true)}))
  ;; and so does the zoom of the source's pages (small print)
  (when-let [z (some-> js/window.localStorage (.getItem doc-zoom-key) js/parseFloat)]
    (when-not (js/isNaN z) (dispatch! {:db #(db/doc-zoomed % z)})))
  ;; and drill-down summaries read in full
  (when (= "true" (some-> js/window.localStorage (.getItem cards-key)))
    (dispatch! {:db #(db/cards-all % true)}))
  ;; and the panes' share of the width
  (when-let [f (some-> js/window.localStorage (.getItem split-key) js/parseFloat)]
    (when-not (js/isNaN f) (dispatch! {:db #(db/split-set % f)})))
  (go-to-route! (history/path->state (history/current-path)))
  (dispatch! {:pull :init}))

;; selecting text offers a reaction to it (robertluo.fly-learner.ui.select)
(defonce _select
  (.addEventListener js/document "mouseup"
                     (fn [^js e]
                       (when-not (.closest (.-target e) ".idea-popover")
                         ;; after the browser has settled the selection
                         (js/setTimeout
                          #(if-let [sel (select/selection-anchor)]
                             (dispatch! {:db (fn [d] (db/selection-made d sel))})
                             (when (get-in @app-db [:ideas :selection])
                               (dispatch! {:db db/selection-cleared})))
                          0)))))

;; full screen follows the browser's: leaving it (Esc, the browser's own
;; controls) leaves the app's too
(defonce _fullscreen
  (do (.addEventListener js/document "fullscreenchange"
                         #(dispatch! {:db (fn [d] (db/fullscreen d (some? (.-fullscreenElement js/document))))}))
      ;; where the browser could not go full screen, Esc still leaves
      (.addEventListener js/document "keydown"
                         (fn [^js e]
                           (when (and (= "Escape" (.-key e)) (:fullscreen? @app-db)
                                      (not (.-fullscreenElement js/document)))
                             (dispatch! {:db #(db/fullscreen % false)}))))))

(defonce _popstate
  (.addEventListener js/window "popstate" (partial history/on-popstate! go-to-route!)))

;; the source pane fits its pages to its width
(defonce _resize
  (let [t (volatile! nil)]
    (.addEventListener js/window "resize"
                       (fn [] (js/clearTimeout @t) (vreset! t (js/setTimeout render! 150))))))

(defonce _init (init!))
