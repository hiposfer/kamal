(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.graph.generators :as g]
            [clojure.test.check.generators :as gen]
            [com.stuartsierra.component :as component]))

(def network (comp osm/complete osm/network))

(def fake-network (comp osm/complete g/with-ways gen/generate g/graph))

(defn start!
  [config]
  (let [result (zipmap (keys (:networks config))
                       (map agent (repeat (count (:networks config)) nil)))
        exec   (if (:dev config) #(time (fake-network (:size %2)))
                                 #(time (network (:osm %2))))]
    (run! (fn [[k cf]] (send-off (k result) exec cf))
          (:networks config))
    result))

(defrecord Router [config networks]
  component/Lifecycle
  (start [this]
    (if (:networks this) this
      (do (println "-- starting Router with" (keys (:networks config)))
          (newline)
          (assoc this :networks (start! config)))))
  (stop [this]
    (println "-- stopping Router")
    (->Router (:config this) nil)))

(defn service
  "returns a Router record that will contain the config
   of and all the networks of the system as agents"
  [] (map->Router {}))
