(ns user
  "REPL development namespace (started by `bb dev`).

    (start!)  backend on http://localhost:8080
    (cljs!)   shadow-cljs watch; app on http://localhost:3000 (proxies /api to :8080)
    (stop!)"
  (:require [learner.server.system :as sys]))

(defn start! [] (sys/start!) :started)
(defn stop! [] (sys/stop!) :stopped)
(defn restart! [] (stop!) (start!))

(defn cljs!
  "Start the shadow-cljs server and watch the :app build."
  []
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
  (require '[learner.ingest :as ingest] '[learner.llm :as llm])
  (def cfg (sys/load-config))
  (def client (llm/client (:anthropic cfg)))
  (ingest/ingest-pdf! (sys/conn) client "data/sources/marmaduke-2024-c2.pdf"
                      {:models (get-in cfg [:anthropic :models])})
  @llm/usage
  (llm/cost))
