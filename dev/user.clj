(ns user
  (:require [learner.main :as main]))

(defn go [] (main/start!) :started)
(defn halt [] (main/stop!) :stopped)
(defn reset [] (halt) (go))

(comment
  (go)
  (halt)

  ;; ingest + fold a PDF into the running system's db
  (require '[learner.ingest :as ingest] '[learner.llm :as llm])
  (def client (llm/client (:anthropic (main/config))))
  (ingest/ingest-pdf! (:conn @main/system) client "data/sources/marmaduke-2024-c2.pdf"))
