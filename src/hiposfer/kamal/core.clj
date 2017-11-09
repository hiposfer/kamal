(ns hiposfer.kamal.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [environ.core :refer [env]]
            [ring.adapter.jetty :as jetty]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.server :as server]
            [hiposfer.kamal.osm :as osm]
            [hiposfer.kamal.graph.generators :as g]
            [clojure.spec.gen.alpha :as gen])
  (:import (org.eclipse.jetty.server Server)))

;; a network created by parsing a OSM file
(defrecord RoadNetwork [config network]
  component/Lifecycle
  (start [this]
    (if (:network this) this
      (let [url     (:osm_url config)
            result  (send-off (agent nil) ;; TODO: error handler
                      (fn [_] (-> (osm/network url)
                                  (osm/neighbourhood)
                                  (time))))]
        (println "-- Starting OSM road network from" url)
        (assoc this :network result))))
  (stop [this]
    (if-not (:network this) this
      (do (println "-- Stopping OSM road network")
          (assoc this :network nil)))))

;; a network created using test.check Clojure generative testing library
(defrecord FakeNetwork [config network]
  component/Lifecycle
  (start [this]
    (if (:network this) this
      (let [size    (or (:network-size config) 1000)
            result  (send-off (agent nil) ;; TODO: error handler
                      (fn [_] (-> (g/graph size)
                                  (gen/generate)
                                  (g/complete)
                                  (osm/neighbourhood)
                                  (time))))]
        (println "-- Starting fake OSM road network with size " size)
        (assoc this :network result))))
  (stop [this]
    (if-not (:network this) this
      (do (println "-- Stopping fake OSM road network")
          (assoc this :network nil)))))

;; --------
;; A Jetty WebServer +  compojure api
(defrecord WebServer [options grid server]
  component/Lifecycle
  (start [this]
    (if (:server this) this
      (let [handler (server/handler grid)
            server  (jetty/run-jetty handler options)]
        (println "-- Starting App server")
        (assoc this :server server))))
  (stop [this]
    (if-let [server (:server this)]
      (do (println "-- Stopping App server")
          (.stop ^Server server)
          (.join ^Server server)
          (assoc this :server nil)))
    this))

(defn system
  "creates a complete system map using components and the provided
  config"
  [config]
  (let [network (if (:dev config) (map->FakeNetwork {})
                    (map->RoadNetwork {}))]
    (component/system-map
      :config config
      :grid   (component/using network
                               [:config])
      :app    (component/using (map->WebServer {})
                               [:config :grid]))))

(defn config
  "returns a configuration for the system based on defaults, environment
  variables and custom options. The configuration will be merged in that
  order. Returns a configuration map for starting a system"
  ([] (config {}))
  ([custom-options]
   (let [default  (edn/read-string (slurp "resources/default-config.edn"))
         env-opts {:port    (edn/read-string (env :port))
                   :join?   (edn/read-string (env :join?))
                   :osm_url (env :osm_url)}]
     (into default (remove (comp nil? second)) ;; ignore nil values
                   (concat env-opts custom-options)))))

(defn -main [& args]
  (println "\n\nWelcome to the hiposfer/kamal App")
  (let [port       (edn/read-string (first args))
        overwrites {:port port}
        config     (config overwrites)]
    (component/start (system config))))