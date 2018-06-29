(ns hiposfer.kamal.services.webserver.core
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.handlers :as handler]
            [ring.adapter.jetty :as jetty]
            [taoensso.timbre :as timbre]
            [compojure.handler :as compojure]
            [ring.middleware.json :as json])
  (:import (org.eclipse.jetty.server Server)))

;; --------
;; A Jetty WebServer +  compojure api
(defrecord WebServer [config router server]
  component/Lifecycle
  (start [this]
    (if (:server this) this
      (let [handler (-> (handler/create router)
                        (compojure/api) ;; standard api middleware
                        (json/wrap-json-response))
            server  (jetty/run-jetty handler {:join? (:JOIN_THREAD config)
                                              :port  (:PORT config)})]
        (timbre/info "-- Starting App server")
        (assoc this :server server))))
  (stop [this]
    (if-let [server (:server this)]
      (do (timbre/info "-- Stopping App server")
          (.stop ^Server server)
          (.join ^Server server)
          (assoc this :server nil router nil)))
    this))

(defn service [] (map->WebServer {}))

