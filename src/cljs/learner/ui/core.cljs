(ns learner.ui.core
  "SPA entry point: the dispatch-of effect pattern (as in flybot.sg).

  dispatch! takes an effect map; effects run in `effect-order`:
    :db      pure state updater (swap! app-db f)
    :pull    named pull op (learner.ui.core.pull), {:pattern .. :then ..},
             or a vector of these (sent in parallel)
    :history :push or :replace the URL for the new state"
  (:require [cognitect.transit :as t]
            [learner.ui.core.db :as db]
            [learner.ui.core.history :as history]
            [learner.ui.core.pull :as pull]
            [learner.ui.core.views :as views]
            [replicant.dom :as r]))

;;=============================================================================
;; API (lasagna-remote over transit+json)
;;=============================================================================

(def ^:private writer (t/writer :json))
(def ^:private reader (t/reader :json {:handlers {"u" (fn [s] (uuid s))}}))

(defn pull!
  "POST a pull pattern to /api. on-success gets the bindings map
  ('sym -> value); on-error gets {:code :reason}."
  [pattern on-success on-error]
  (-> (js/fetch "/api" #js {:method  "POST"
                            :headers #js {"Content-Type" "application/transit+json"
                                          "Accept"       "application/transit+json"}
                            :body    (t/write writer {:pattern pattern})})
      (.then (fn [^js resp]
               (-> (.text resp)
                   (.then (fn [text]
                            (let [body (when (seq text) (t/read reader text))]
                              (if (.-ok resp)
                                (on-success body)
                                (on-error (or (first (:errors body))
                                              {:code :unknown :reason (str "HTTP " (.-status resp))})))))))))
      (.catch (fn [e] (on-error {:code :network :reason (str e)})))))

;;=============================================================================
;; Dispatch
;;=============================================================================

(defonce app-db (atom db/initial-state))

(def ^:private effect-order [:db :pull :history])

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
                         (when-let [{:keys [pattern then]}
                                    (if (map? op) op (pull/resolve-pull op @app-db))]
                           (pull! pattern
                                  #(@self (then %))
                                  #(@self {:db (fn [d] (db/set-error d %))}))))
              :history (if (= :replace effect)
                         (history/replace-state! @app-db)
                         (history/push-state! @app-db)))))]
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

(defonce _render-on-change
  (add-watch app-db ::render
             (fn [_ _ old new]
               (render!)
               (when (not= (select-keys old [:view :node-id :ask-id :keyword])
                           (select-keys new [:view :node-id :ask-id :keyword]))
                 (js/window.scrollTo 0 0)))))

;;=============================================================================
;; Init
;;=============================================================================

(defn- go-to-route! [route]
  (dispatch! {:db #(db/apply-route % route) :pull (pull/for-route route)}))

(defn ^:export init! []
  (go-to-route! (history/path->state (history/current-path)))
  (dispatch! {:pull :init}))

(defonce _popstate
  (.addEventListener js/window "popstate" (partial history/on-popstate! go-to-route!)))

;; the source pane fits the PDF page to its width
(defonce _resize
  (let [t (volatile! nil)]
    (.addEventListener js/window "resize"
                       (fn [] (js/clearTimeout @t) (vreset! t (js/setTimeout render! 150))))))

(defonce _init (init!))
