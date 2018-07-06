(ns hiposfer.kamal.services.webserver.core
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.handlers :as handler]
            [ring.adapter.jetty :as jetty]
            [taoensso.timbre :as timbre]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.nested-params :refer [wrap-nested-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :as json]
            [ring.util.http-response :as code]
            [ring.middleware.accept :as accept]
            [hiposfer.kamal.libs.tool :as tool])
  (:import (org.eclipse.jetty.server Server)))

(defn- inject-networks
  "inject the networks (agent current value) into the request"
  [handler conn]
  (fn inject-networks*
    [request]
    (let [regions @conn]
      (if (empty? regions)
        (code/service-unavailable "routers have not started yet")
        (handler (assoc request :kamal/networks regions))))))

(defn- shape-response
  [handler]
  (fn shape-response*
    [request]
    (let [response (handler request)]
      (if (string? (response :body))
        response
        (case (:mime (:accept request))
          "application/json" (update response :body tool/json-namespace)
          "application/edn" (update response :body pr-str))))))

;; --------
;; A Jetty WebServer +  compojure api
(defrecord WebServer [config router server]
  component/Lifecycle
  (start [this]
    (if (:server this) this
      (let [handler (-> (handler/create)
                        (inject-networks (:networks router))
                        (shape-response)
                        (accept/wrap-accept {:mime ["application/json" "application/edn"]})
                        (json/wrap-json-response) ;; note efficient but works
                        (json/wrap-json-params)
                        (wrap-keyword-params)
                        (wrap-nested-params)
                        (wrap-params))
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
