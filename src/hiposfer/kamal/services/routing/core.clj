(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.graph.generators :as g]
            [clojure.test.check.generators :as gen]))

(def network (comp osm/neighbourhood osm/network))

(def fake-network (comp osm/neighbourhood g/complete gen/generate g/graph))

(defn service
  [config]
  (let [result (agent nil)
        size   (or (:network-size config) 1000)
        url    (:osm_url config)
        exec!  (if (:dev config) #(time (fake-network size))
                                 #(time (network url)))]
    (println "-- Starting road network with" config)
    (send-off result exec!)))
