(ns user
  "REPL development namespace (started by `bb dev`).

    (start!)  backend on http://localhost:8080
    (cljs!)   backend (if not running) + shadow-cljs watch; app on
              http://localhost:3000, which proxies pages and /api to :8080
    (stop!)"
  (:require [robertluo.fly-learner.server.system :as sys]))

(defn start!
  "Start the backend unless it is running."
  []
  (if @sys/system :already-started (do (sys/start!) :started)))

(defn stop! [] (sys/stop!) :stopped)
(defn restart! [] (stop!) (start!))

(defn cljs!
  "Start the backend (:3000 proxies to it) and the shadow-cljs server, and
  watch the :app build."
  []
  (start!)
  ((requiring-resolve 'shadow.cljs.devtools.server/start!))
  ((requiring-resolve 'shadow.cljs.devtools.api/watch) :app))

(comment
  (start!)
  (cljs!)
  (stop!)

  ;; query the API like the frontend does
  (require '[sg.flybot.pullable.remote.client :as client])
  (def api (client/connect "http://localhost:8080/api"))
  (api '{:sources ?sources})

  ;; ingest + fold a PDF into the running system's db
  (require '[robertluo.fly-learner.ingest :as ingest] '[robertluo.fly-learner.llm :as llm])
  (def cfg (sys/load-config))
  (def client (llm/client (:anthropic cfg)))
  (ingest/ingest-pdf! (sys/conn) client "data/sources/marmaduke-2024-c2.pdf"
                      {:models (get-in cfg [:anthropic :models])})
  @llm/usage
  (llm/cost)

  ;; concepts: resolve keywords no concept covers yet, then explore the graph
  (require '[robertluo.fly-learner.concept :as concept]
           '[robertluo.fly-learner.concept.graph :as graph]
           '[datalevin.core :as d])
  (concept/resolve! (sys/conn) (concept/llm-resolver client {:models (get-in cfg [:anthropic :models])})
                    {:on-batch prn})
  (def adj (graph/edges (graph/load-graph (d/db (sys/conn)))))
  (concept/edits (d/db (sys/conn))))
