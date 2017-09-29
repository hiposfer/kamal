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

;; a Road network created by parsing a OSM file
(defrecord RoadNetwork [url network]
  component/Lifecycle
  (start [this]
    (if (:network this) this
      (let [result (future (time (osm/osm->network url)))]
        (println "-- Starting OSM road network from" url)
        (assoc this :network result))))
  (stop [this]
    (if-not (:network this) this
      (do (println "-- Stopping OSM road network")
          (assoc this :network nil)))))

;; a fake Road network created using test.check Clojure generative testing library
(defrecord FakeNetwork [size network]
  component/Lifecycle
  (start [this]
    (if (:network this) this
      (let [result (future (time (g/complete (gen/generate (g/graph size)))))]
        (println "-- Starting fake OSM road network with size " size)
        (assoc this :network result))))
  (stop [this]
    (if-not (:network this) this
      (do (println "-- Stopping fake OSM road network")
          (assoc this :network nil)))))

(defn network
  "creates a network component map based on the given configuration"
  [config]
  (if (:dev config)
    (map->FakeNetwork {:size (or (:network-size config) 1000)})
    (map->RoadNetwork {:url (:osm-url config)})))

;; --------
;; Our Server application. Uses the jetty adapter with
;; compojure api
(defrecord AppServer [options grid]
  component/Lifecycle
  (start [this]
    (if (:server this) this
      (let [handler (server/create grid)
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

;; the complete system
(defn system [config]
  (component/system-map
    :config-options config
    :grid (network config)
    :app (component/using
           (map->AppServer {:options config})
           [:grid])))
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
         env-opts {:port    (edn/read-string (env :port))
                   :join?   (edn/read-string (env :join?))
                   :osm-url (env :osm-url)}]
     (merge default
            (into {} (remove (comp nil? second) env-opts)) ;; ignore nil values
            custom-options))))

(defn -main [& args]
  (println "\n\nWelcome to the hiposfer/kamal App")
  (let [config     (config)
        port       (edn/read-string (first args))
        overwrites (into {} (remove (comp nil? second))
                            {:port port}) ;; ignore nil values
        new-config (merge config overwrites)]
    (component/start (system new-config))))