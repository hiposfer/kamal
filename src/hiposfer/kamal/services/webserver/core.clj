(ns hiposfer.kamal.services.webserver.core
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.handlers :as handler]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as rut]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.nested-params :refer [wrap-nested-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :as json]
            [hiposfer.kamal.services.webserver.edn :as edn]
            [ring.util.http-response :as code]
            [ring.middleware.accept :as accept]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.walk :as walk])
  (:import (org.eclipse.jetty.server Server)
           (clojure.lang ExceptionInfo)))

(defn- inject-networks
  "inject the networks (agent current value) into the request"
  [handler conn]
  (fn inject-networks*
    [request]
    (let [networks @conn]
      (if (empty? networks)
        (code/service-unavailable "routers have not started yet")
        (handler (assoc request :kamal/networks networks))))))

(defn- shape-response
  [handler]
  (fn shape-response*
    [request]
    (let [response (handler request)]
      (if (not (coll? (response :body)))
        response
        (case (:mime (:accept request))
          "application/json"
          (update response :body #(walk/postwalk tool/jsonista %))

          "application/edn"
          (-> (update response :body pr-str)
              (rut/content-type "application/edn; charset=utf-8")))))))

(defn wrap-exceptions
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch ExceptionInfo e ;; let Jetty catch any other exception
        ;;(stack/print-stack-trace e)
        (cond
          ;; standard ring ex-info exception -> most likely thrown by us
          (= ::code/response (:type (ex-data e))) (:response (ex-data e))
          ;; raw ex-info -> return its data directly
          (contains? (ex-data e) :status) (code/bad-request (ex-data e))
          ;; something else happened -> internal server error
          :else (code/internal-server-error {:msg (or (.getMessage e)
                                                      (ex-data e))}))))))

;; .........................................................
;; A Jetty WebServer +  compojure api
(defrecord WebServer [config router server]
  component/Lifecycle
  (start [this]
    (if (:server this) this
      ;; this is not the standard way of composing middleware but it feels more
      ;; natural to me ...
      ;; request top -> bottom
      ;; response bottom -> up
      (let [handler (wrap-params
                      (wrap-nested-params
                        (wrap-keyword-params
                          (edn/wrap-body
                            (json/wrap-json-body
                              (json/wrap-json-response
                                (accept/wrap-accept
                                  (wrap-exceptions
                                    (shape-response
                                      (inject-networks handler/server (:networks router))))
                                  {:mime ["application/json" "application/edn"]})))))))
            server  (jetty/run-jetty handler {:join? (:JOIN_THREAD config)
                                              :port  (:PORT config)})]
        (println "-- Starting App server")
        (assoc this :server server))))
  (stop [this]
    (if-let [server (:server this)]
      (do (println "-- Stopping App server")
          (.stop ^Server server)
          (.join ^Server server)
          (assoc this :server nil router nil)))
    this))

(defn service [] (map->WebServer {}))
