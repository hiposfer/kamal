(ns hiposfer.kamal.services.webserver.core
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.handlers :as handler]
            [ring.adapter.jetty :as jetty]
            [taoensso.timbre :as timbre]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.nested-params :refer [wrap-nested-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :as json]
            [ring.middleware.edn :as medn]
            [ring.util.http-response :as code]
            [clojure.walk :as walk])
  (:import (org.eclipse.jetty.server Server)
           (java.time LocalDateTime DayOfWeek LocalDate ZoneId)))

(def customs #{ZoneId LocalDateTime LocalDate DayOfWeek})

(defn- verbose
  [v]
  (if (not (some #(instance? % v) customs)) v
    {:type (.getCanonicalName ^Class (.getClass ^Object v))
     :value (str v)}))

(defn- json-type
  "transform registered Classes into a :type :value map to
  identify its type at the other end"
  [handler]
  (fn json-type*
    [request]
    (let [response (handler request)]
      (if (coll? (:body response))
        (assoc response :body (walk/postwalk verbose (:body response)))
        response))))

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
                        (json-type)
                        (inject-networks (:networks router))
                        (medn/wrap-edn-params)
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

