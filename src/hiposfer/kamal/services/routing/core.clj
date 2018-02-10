(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.generators :as ng]
            [clojure.test.check.generators :as gen]
            [com.stuartsierra.component :as component]))

(def network (comp osm/complete osm/network))

(def fake-network (comp osm/complete ng/with-ways gen/generate ng/graph))

(defn start!
  [config]
  (let [exec   (if (:dev config)
                 #(time (fake-network (:size %2)))
                 #(time (network (:osm %2))))]
    (for [region (:networks config)]
      (send-off (agent nil) exec region))))

(defrecord Router [config networks]
  component/Lifecycle
  (start [this]
    (if (not-empty (:networks this)) this
      (do (println "-- starting Router with" (:networks config))
          (newline)
          (assoc this :networks (doall (start! config))))))
  (stop [this]
    (println "-- stopping Router")
    (assoc this :networks nil)))

(defn service
  "returns a Router record that will contain the config
   of and all the networks of the system as agents"
  [] (map->Router {}))
