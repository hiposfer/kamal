(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.generators :as ng]
            [clojure.test.check.generators :as gen]
            [datascript.core :as data]
            [com.stuartsierra.component :as component]))

;(def fake-network (comp osm/complete ng/with-ways gen/generate ng/graph))

;; NOTE: we only model outgoing links of the graph. If you are interested
;; in the incoming ones, use a query to find those. It wont be as performant
;; a direct lookup but I guess that if you are using incoming links, you must
;; know what you are doing ;)

(def schema {;; Open Street Map - entities
             :node/id         {:db.unique :db.unique/identity}
             :node/lat        {:db/index true}
             :node/lon        {:db/index true}
             :node/neighbours {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/many}

             :way/id         {:db.unique :db.unique/identity}

             ;; General Transfer Feed Specification - entities
             :trip/id        {:db.unique :db.unique/identity}
             :trip/route     {:db/type :db.type/ref}
             :trip/service   {:db/type :db.type/ref}

             :agency/id      {:db.unique :db.unique/identity}

             :service/id     {:db.unique :db.unique/identity}

             :route/id       {:db.unique :db.unique/identity}
             :route/agency   {:db/type :db.type/ref}

             :stop/id        {:db.unique :db.unique/identity}
             :stop/lat       {:db/index true}
             :stop/lon       {:db/index true}

             :stop.time/trip {:db/type :db.type/ref}
             :stop.time/stop {:db/type :db.type/ref}})

(defn start!
  [config]
  (let [exec   (if (:dev config)
                 ;#(time (fake-network (:size %2)))
                 #(time (osm/datomize (:osm %2))))]
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

;(def network (time (osm/datomize "resources/osm/saarland.min.osm.bz2")))

;(def conn (data/create-conn schema))

;(take-last 5 (data/datoms @conn :eavt))
;(data/entity @conn [:trip_id 406014.151])

;(let [a (time (data/transact! conn network))]
;  (take-last 5 (data/datoms @conn :eavt)))

;(take 10 (data/datoms @conn :eavt))

;(time (data/q '[:find (pull ?route [*]) .
;                :where [?route :route/id 450854]]
;               @conn))

;(take-last 10 (data/datoms @conn :eavt))

;(take-last 10 network)
