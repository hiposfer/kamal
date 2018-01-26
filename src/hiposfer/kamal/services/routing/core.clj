(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.graph.generators :as g]
            [clojure.test.check.generators :as gen]
            [com.stuartsierra.component :as component]))

(def network (comp osm/neighbourhood osm/network))

(def fake-network (comp osm/neighbourhood g/complete gen/generate g/graph))

(defn start!
  [config]
  (let [result (zipmap (keys (:networks config))
                       (map agent (vals (:networks config))))
        exec   (if (:dev config) #(time (fake-network (:size %)))
                                 #(time (network (:osm %))))]
    (run! (fn [ag] (send-off ag exec))
          (vals result))
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
