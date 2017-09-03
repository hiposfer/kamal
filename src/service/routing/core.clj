(ns service.routing.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [environ.core :refer [env]]
            [ring.adapter.jetty :as jetty]
            [com.stuartsierra.component :as component]
            [service.routing.server :as server])
  (:import (org.eclipse.jetty.server Server)))

(defrecord AppServer [handler options]
  component/Lifecycle
  (start [component]
    (if (:server component)
      component
      (let [server (jetty/run-jetty handler options)]
        (println ";; Starting App server")
        (assoc component :server server))))
  (stop [component]
    (if-let [server (:server component)]
      (do (println ";; Stopping App server")
          (.stop ^Server server)
          (.join ^Server server)
          (assoc component :server nil)))
    component))

(defn system [config-options]
  (component/system-map
    :config-options config-options
    ;:db (new-database host port)
    :app (map->AppServer {:handler server/app :options config-options})))
  ;(component/system-using
  ;   {:app {:database  :db}
  ;          :scheduler :sched)))))

(defn -main [& args]
  (println "Welcome to the org.n7a235/service.routing App")
  (let [port (or (edn/read-string (first args))
                 (env :port))
        join (or (edn/read-string (env :join?)) true)]
    (assert port "no port provided. Either set an ENV var or pass it as an argument")
    (component/start (system {:port port :join? join}))))