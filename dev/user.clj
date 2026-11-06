(ns user
  (:require [learner.main :as main]))

(defn go [] (main/start!) :started)
(defn halt [] (main/stop!) :stopped)
(defn reset [] (halt) (go))

(comment
  (go)
  (halt)

  ;; ingest + fold a PDF
  (require '[learner.ingest.pdf :as pdf] '[learner.fold :as fold] '[learner.llm :as llm])
  (def doc (pdf/extract "data/sources/example.pdf"))
  (def nodes (fold/link-by-keywords
              (fold/fold (fold/llm-summarizer (llm/client (:anthropic (main/config)))) (:paragraphs doc)))))
