(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.generators :as ng]
            [clojure.test.check.generators :as gen]
            [hiposfer.kamal.parsers.gtfs.core :as gtfs]
            [datascript.core :as data]
            [com.stuartsierra.component :as component]))

;; NOTE: we only model outgoing links of the graph. If you are interested
;; in the incoming ones, use a query to find those. It wont be as performant
;; a direct lookup but I guess that if you are using incoming links, you must
;; know what you are doing ;)

;; NOTE: we actually have double side relations in the database. Meaning that
;; if there is a bidirectional connection between a and b then both of them will
;; have their neighbours id stored in the db. This is due to speed. A single
;; entity lookup takes around 200 microseconds, while a query for those takes
;; around 50 milliseconds. For graph traversals that is unacceptable !!
;; This however goes against Datomic best practices :(

;; NOTE: we use a node/location instead of lat, lon separate to avoid creating
;; so many datoms and to have a single entity that we can apply protocols against.
;; This is a hack !! but it works :)
;; Thanks datascript

(def schema {;; Open Street Map - entities
             :node/id         {:db.unique :db.unique/identity}
             :node/location   {:db/index true}
             :node/successors {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/many}

             :way/id          {:db.unique :db.unique/identity}
             :way/nodes       {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/many}

             ;; General Transfer Feed Specification - entities
             :trip/id         {:db.unique :db.unique/identity}
             :trip/route      {:db/type :db.type/ref}
             :trip/service    {:db/type :db.type/ref}

             :agency/id       {:db.unique :db.unique/identity}

             :service/id      {:db.unique :db.unique/identity}

             :route/id        {:db.unique :db.unique/identity}
             :route/agency    {:db/type :db.type/ref}

             :stop/id         {:db.unique :db.unique/identity}
             :stop/lat        {:db/index true}
             :stop/lon        {:db/index true}

             :stop.time/trip  {:db/type :db.type/ref}
             :stop.time/stop  {:db/type :db.type/ref}})

;; TODO: link OSM nodes with GTFS stops
(defn exec!
  "builds a datascript in-memory db and conj's it into the passed agent"
  [ag area dev]
  (println "-- starting area router:" area)
  (let [vehicle     (when (and (not dev) (:gtfs area))
                      (future (gtfs/datomize (:gtfs area))))
        pedestrian  (if (true? dev)
                      (concat (gen/generate (ng/graph (:size area)))
                              (gen/generate (ng/ways (:size area))))
                      (osm/datomize (:osm area)))
        conn  (data/create-conn schema)]
    (data/transact! conn (concat pedestrian @vehicle))
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

;(def conn (data/create-conn schema))

;(time (map :v (take 5 (data/index-range @conn :node/location [6.9513 49.318267] nil))))

;(let [a (time (data/transact! conn network))]
 ; (take-last 5 (data/datoms @conn :eavt)))
