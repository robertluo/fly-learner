(ns robertluo.fly-learner.server.system
  "System lifecycle with fun-map's life-cycle-map (as in flybot.sg).

    (def sys (make-system (load-config)))
    (::http-server sys)   ; touching a component starts its dependency chain
    (halt! sys)           ; stops everything in reverse order"
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datalevin.core :as d]
            [robertluo.fly-learner.db :as db]
            [robertluo.fly-learner.ideas.store :as ideas.store]
            [robertluo.fly-learner.explore.store :as explore.store]
            [robertluo.fly-learner.learning :as learning]
            [robertluo.fly-learner.learning.concepts :as learning.concepts]
            [robertluo.fly-learner.learning.decay :as decay]
            [robertluo.fly-learner.learning.store :as learning.store]
            [robertluo.fly-learner.llm :as llm]
            [robertluo.fly-learner.quiz :as quiz]
            [robertluo.fly-learner.server.api :as api]
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
   [:anthropic {:optional true} :map]])

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
  (or (#{"/" "/c" "/q" "/x"} uri) (some #(str/starts-with? uri %) ["/n/" "/k/" "/a/" "/c/" "/g/" "/x/"])))

(defn- source-file
  "GET /files/sources/<uuid> -> the source's original file (only paths stored
  by ingest are ever served)."
  [conn uri]
  (when-let [id (some-> (re-matches #"/files/sources/([0-9a-f-]{36})" uri) second parse-uuid)]
    (when-let [path (:source/path (db/source (d/db conn) id))]
      (when (.isFile (io/file path))
        (-> (resp/file-response path)
            (resp/content-type "application/pdf")
            (resp/header "Cache-Control" "private, max-age=3600"))))))

(defn make-ring-app [conn api-fn]
  (-> (fn [{:keys [uri request-method]}]
        (or (when (= :get request-method)
              (or (source-file conn uri)
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
    (fnk [::cfg] {:client (delay (llm/client (:anthropic cfg)))})

    ::api-fn
    (fnk [::cfg ::db ::llm ::learning]
         (api/make-api (:conn db) (:client llm) {:models (get-in cfg [:anthropic :models])} learning))

    ::ring-app
    (fnk [::db ::api-fn] (make-ring-app (:conn db) api-fn))

    ::http-server
    (fnk [::cfg ::ring-app]
         (let [port (get-in cfg [:http :port])
               stop (http-kit/run-server ring-app {:port port})]
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
