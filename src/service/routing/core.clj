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

(defn config
  "returns a configuration for the system based on defaults, environment
  variables and custom options. The configuration will be merged in that
  order. Returns a configuration map for starting a system"
  ([] (config {}))
  ([custom-options]
   (let [default  (edn/read-string (slurp "resources/default-config.edn"))
         env-opts {:port     (edn/read-string (env :port))
                   :join?    (edn/read-string (env :join?))
                   :osm-file (env :osm-file)}]
     (merge default
            (into {} (remove (comp nil? second) env-opts)) ;; ignore nil values
            custom-options))))

(defn -main [& args]
  (println "Welcome to the org.n7a235/service.routing App")
  (let [config     (config)
        port       (edn/read-string (first args))
        new-config (if (not port) config
                     (merge config {:port port}))]
    (component/start (system new-config))))