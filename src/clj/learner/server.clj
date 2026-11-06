(ns learner.server
  "HTTP API consumed by the ClojureScript frontend."
  (:require [datalevin.core :as d]
            [learner.db :as db]
            [muuntaja.core :as m]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [ring.adapter.jetty :as jetty]))

(defn- ok [body] {:status 200 :body body})

(defn routes [{:keys [conn]}]
  [["/api"
    ["/sources" {:get (fn [_] (ok (db/sources (d/db conn))))}]
    ["/nodes/:id" {:get (fn [{{:keys [id]} :path-params}]
                          (if-let [n (db/node (d/db conn) (parse-uuid id))]
                            (ok n)
                            {:status 404 :body {:error "not found"}}))}]]])

(defn handler [ctx]
  (ring/ring-handler
   (ring/router (routes ctx)
                {:data {:muuntaja   m/instance
                        :middleware [muuntaja/format-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/"})
    (ring/create-default-handler))))

(defn start [ctx {:keys [port]}]
  (jetty/run-jetty (handler ctx) {:port port :join? false}))

(defn stop [server]
  (.stop ^org.eclipse.jetty.server.Server server))
