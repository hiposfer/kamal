(ns hiposfer.kamal.services.webserver.core
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.handlers :as handler]
            [ring.adapter.jetty :as jetty]
            [taoensso.timbre :as timbre]
            [cheshire.generate :as cheshire]
            [cheshire.custom :as custom]
            [compojure.handler :as compojure]
            [ring.middleware.json :as json]
            [ring.util.http-response :as code])
  (:import (org.eclipse.jetty.server Server)
           (java.time ZoneRegion LocalDateTime DayOfWeek)))

;; add custom encoders for Java Time classes
(cheshire/add-encoder ZoneRegion custom/encode-str)
(cheshire/add-encoder LocalDateTime custom/encode-str)
(cheshire/add-encoder DayOfWeek custom/encode-str)

(defn- inject-networks
  "inject the networks (agent current value) into the request"
  [handler conn]
  (fn inject-networks*
    [request]
    (let [regions @conn]
      (if (empty? regions)
        (code/service-unavailable "routers have not started yet")
        (handler (assoc request :kamal/networks regions))))))

;; --------
;; A Jetty WebServer +  compojure api
(defrecord WebServer [config router server]
  component/Lifecycle
  (start [this]
    (if (:server this) this
      (let [handler (-> (handler/create)
                        (inject-networks (:networks router))
                        (json/wrap-json-response)
                        (compojure/api))
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

