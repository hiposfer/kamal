(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.generators :as ng]
            [clojure.test.check.generators :as gen]
            [hiposfer.kamal.parsers.gtfs.core :as gtfs]
            [datascript.core :as data]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.network.algorithms.core :as alg]))

;; NOTE: we use :db/index true to replace the lack of :VAET index in datascript
;; This is for performance. In lots of cases we want to lookup back-references
;; to a certain datom. Without indexes it is not possible to find it without
;; traversing most (all?) of the db.
;; The same kind of behavior can be achieved by combining entity with index-range
;; such that the queries can still be answered pretty fast :)

;; NOTE: we use a node/location instead of lat, lon separate to avoid creating
;; so many datoms and to have a single entity that we can apply protocols against.
;; This is a hack !! but it works :)
;; Thanks datascript

(def schema {;; Open Street Map - entities
             :node/id         {:db.unique :db.unique/identity}
             :node/location   {:db/index true}
             :node/successors {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/many
                               :db/index true}

             :way/id          {:db.unique :db.unique/identity}
             :way/nodes       {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/many
                               :db/index true}

             ;; General Transfer Feed Specification - entities
             :trip/id         {:db.unique :db.unique/identity}
             :trip/route      {:db/type :db.type/ref}
             :trip/service    {:db/type :db.type/ref}

             :agency/id       {:db.unique :db.unique/identity}

             :service/id      {:db.unique :db.unique/identity}

             :route/id        {:db.unique :db.unique/identity}
             :route/agency    {:db/type :db.type/ref}

             :stop/id         {:db.unique :db.unique/identity}
             :stop/location   {:db/index true}

             :stop.time/trip  {:db/type :db.type/ref}
             :stop.time/stop  {:db/type :db.type/ref}})

;; TODO: link OSM nodes with GTFS stops
(defn exec!
  "builds a datascript in-memory db and conj's it into the passed agent"
  [ag area dev]
  (println "-- starting area router:" area)
  (let [vehicle     (when (and (not dev) (:gtfs area))
                      (time (gtfs/datomize (:gtfs area))))
        pedestrian  (if (true? dev)
                      (let [graph (gen/generate (ng/graph (:size area)))]
                        (concat graph (ng/ways (alg/node-ids graph))))
                      (time (osm/datomize (:osm area))))
        conn  (data/create-conn schema)]
    (time (data/transact! conn (concat pedestrian vehicle)))
    (conj ag conn)))

(defrecord Router [config networks]
  component/Lifecycle
  (start [this]
    (if (not-empty (:networks this)) this
      (let [ag (agent #{})]
        (run! #(send-off ag exec! % (:dev config))
               (:networks config))
        (assoc this :networks ag))))
  (stop [this]
    (println "-- stopping router")
    (assoc this :networks nil)))

(defn service
  "returns a Router record that will contain the config
   of and all the networks of the system as agents"
  [] (map->Router {}))

;(def network (time (osm/datomize "resources/osm/saarland.min.osm.bz2")))
;(def network nil)

;(def conn (data/create-conn schema))

;(time (map :v (take 5 (data/index-range @conn :node/location [6.9513 49.318267] nil))))

;(let [a (time (data/transact! conn network))]
;  (take-last 5 (data/datoms @conn :eavt)))

;(time (into {} (data/entity @conn 202982)))

;(take 5 (data/datoms @conn :eavt))
;(time ; aevt
;  (take-while #(= (:v %) 1) (data/index-range @conn :way/nodes 1 nil)))
