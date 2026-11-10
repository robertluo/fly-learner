(ns robertluo.fly-learner.server.system
  "System lifecycle with fun-map's life-cycle-map (as in flybot.sg).

    (def sys (make-system (load-config)))
    (::http-server sys)   ; touching a component starts its dependency chain
    (halt! sys)           ; stops everything in reverse order"
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ideas.store :as ideas.store]
            [robertluo.fly-learner.explore.store :as explore.store]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.learning.concepts :as learning.concepts]
            [robertluo.fly-learner.learning.decay :as decay]
            [robertluo.fly-learner.learning.store :as learning.store]
            [robertluo.fly-learner.prompts :as prompts]
            [robertluo.fly-learner.quiz :as quiz]
            [robertluo.fly-learner.server.api :as api]
            [robertluo.fly-learner.server.files :as files]
            [robertluo.fly-learner.server.ingests :as ingests]
            [robertluo.fly-learner.server.settings :as settings]
            [malli.core :as m]
            [malli.error :as me]
            [org.httpkit.server :as http-kit]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as resp]
            [robertluo.fun-map :refer [closeable fnk halt! life-cycle-map]]
            [sg.flybot.pullable.remote :as remote]))

;;=============================================================================
;; Config
;;=============================================================================

(def Config
  [:map
   [:db-dir :string]
   [:http [:map [:port :int]]]
   [:prompts-file {:optional true} :string] ; the learner's versions of the prompts (data/prompts.edn)
   [:anthropic {:optional true} :map] ; the older form of :llm: Anthropic alone
   [:llm {:optional true}
    [:map
     [:providers {:optional true}
      [:map-of :keyword [:map [:type [:enum :anthropic :openai :ollama]]]]]
     [:default {:optional true} :keyword]
     [:models {:optional true} [:map-of :keyword :string]]]]])

(defn load-config
  "resources/config.edn, validated."
  []
  (let [cfg (-> (io/resource "config.edn") slurp edn/read-string)]
    (when-not (m/validate Config cfg)
      (throw (ex-info "Invalid config.edn" {:errors (me/humanize (m/explain Config cfg))})))
    cfg))

;;=============================================================================
;; Ring app
;;=============================================================================

(defn- spa-route?
  "Client-side routes served by index.html (see robertluo.fly-learner.ui.core.history)."
  [uri]
  (or (#{"/" "/b" "/c" "/q" "/s" "/x"} uri) (some #(str/starts-with? uri %) ["/n/" "/k/" "/t/" "/a/" "/c/" "/g/" "/x/"])))

(defn make-ring-app
  "The app: the API, a source's files, uploads to add sources (`upload-fn`:
  ring request -> response), index.html for the UI's routes."
  [conn api-fn upload-fn]
  (-> (fn [{:keys [uri request-method] :as req}]
        (or (when (and (= :post request-method) (= "/files/upload" uri))
              (upload-fn req))
            (when (= :get request-method)
              (or (files/response conn uri)
                  (when (spa-route? uri)
                    (-> (resp/resource-response "public/index.html")
                        (resp/content-type "text/html; charset=utf-8")))))
            (resp/not-found "Not found")))
      (remote/wrap-api api-fn {:path "/api"})
      (wrap-resource "public")
      wrap-content-type
      wrap-not-modified))

;;=============================================================================
;; System
;;=============================================================================

(defn connect
  "Open the db at the config's :db-dir with every layer's schema (each layer
  owns its own)."
  [cfg]
  ;; the learner's versions of the prompts, kept beside the db
  (prompts/use-file! (or (:prompts-file cfg) "data/prompts.edn"))
  (db/connect (:db-dir cfg) learning.store/schema quiz/schema ideas.store/schema explore.store/schema))

(defn make-system [cfg]
  (life-cycle-map
   {::cfg cfg

    ::db
    (fnk [::cfg]
         (let [conn (connect cfg)]
           ;; closeable wraps in a map, so fun-map won't auto-deref the conn inside
           (closeable {:conn conn} #(db/close conn))))

    ;; how learning is wired: the model (decay is an addition to mastery,
    ;; chosen here) and the curriculum (the concept graph, rebuilt per db value)
    ::learning
    (fnk [] {:model      (decay/decaying (learning/mastery))
             :curriculum learning.concepts/curriculum})

    ;; the client is built on the first ask, so the server runs without an API
    ;; key until then; in a map, since fun-map would deref a bare delay
    ::llm
    ;; the settings in force (config.edn under those edited in the app), the client rebuilt when they change
    (fnk [::cfg] {:client (settings/client-ref cfg)})

    ::api-fn
    (fnk [::cfg ::db ::llm ::learning]
         (api/make-api (:conn db) (:client llm) cfg learning))

    ::ring-app
    (fnk [::cfg ::db ::api-fn]
         (make-ring-app (:conn db) api-fn
                        (fn [req] (ingests/upload-response (:conn db) (:models (settings/llm cfg)) req))))

    ::http-server
    (fnk [::cfg ::ring-app]
         (let [port (get-in cfg [:http :port])
               ;; a dropped source comes whole in the request body
               stop (http-kit/run-server ring-app {:port port :max-body (+ ingests/max-upload (* 1024 1024))})]
           (println "fly-learner listening on" (str "http://localhost:" port))
           (closeable {:port port} #(stop :timeout 100))))}))

(defonce ^{:doc "The running system, or nil."} system (atom nil))

(defn start!
  ([] (start! (load-config)))
  ([cfg]
   (let [sys (make-system cfg)]
     (::http-server sys)
     (reset! system sys))))

(defn stop! []
  (when-let [sys @system]
    (halt! sys)
    (reset! system nil)))

(defn conn
  "Datalevin connection of the running system."
  []
  (some-> @system ::db :conn))

(defn -main [& _]
  (start!)
  (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop!)))
