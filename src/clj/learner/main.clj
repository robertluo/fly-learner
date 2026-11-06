(ns learner.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [learner.db :as db]
            [learner.server :as server])
  (:gen-class))

(defn config []
  (-> (io/resource "config.edn") slurp edn/read-string))

(defonce system (atom nil))

(defn start! []
  (let [{:keys [db-dir http]} (config)
        conn (db/connect db-dir)]
    (reset! system {:conn conn :server (server/start {:conn conn} http)})
    (println "learner listening on port" (:port http))))

(defn stop! []
  (when-let [{:keys [conn server]} @system]
    (server/stop server)
    (db/close conn)
    (reset! system nil)))

(defn -main [& _]
  (start!))
