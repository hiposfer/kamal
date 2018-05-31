(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.generators :as ng]
            [clojure.test.check.generators :as gen]
            [hiposfer.kamal.parsers.gtfs.core :as gtfs]
            [datascript.core :as data]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.libs.fastq :as fastq]
            [taoensso.timbre :as timbre]
            [clojure.java.io :as io]
            [hiposfer.kamal.parsers.edn :as edn])
  (:import (java.util.zip ZipInputStream)))

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
             :stop/successors {:db.type :db.type/ref
                               :db.cardinality :db.cardinality/many}
             :stop/location   {:db/index true}

             :stop.times/trip  {:db/type :db.type/ref
                                :db/index true}
             :stop.times/stop  {:db/type :db.type/ref
                                :db/index true}})

(defn network
  "builds a datascript in-memory db and conj's it into the passed agent"
  [area]
  (timbre/info "starting area router:" area)
  (if (:area/edn area)
    (data/conn-from-db (edn/parse (:area/edn area)))
    (let [conn        (data/create-conn schema)
          pedestrian  (osm/datomize (:area/osm area))
          network     (data/db-with @conn pedestrian)
          network2    (if (not (:area/gtfs area)) network
                        (with-open [z (-> (io/input-stream (:area/gtfs area))
                                          (ZipInputStream.))]
                          (data/db-with network (gtfs/datomize! z))))
          network3    (data/db-with network2 (fastq/link-stops network2))
          network4    (data/db-with network3 (fastq/cache-stop-successors network3))]
      (doto conn (reset! network4)))))

(defn pedestrian-graph
  "builds a datascript in-memory db and returns it. Only valid for
  pedestrian routing"
  [area]
  ;; we dont support fake GTFS data for development yet
  (let [conn   (data/create-conn schema)
        size   (or (:SIZE area) 100)
        g      (gen/such-that not-empty (ng/graph size) 1000)
        graph  (data/db-with @conn (gen/generate g))
        graph  (data/db-with graph (ng/ways (map :node/id (alg/nodes graph))))]
    (doto conn (reset! graph))))

(defrecord DevRouter [config networks]
  component/Lifecycle
  (start [this]
    (timbre/info "starting dev router with:" config)
    (if (not-empty (:networks this)) this
      (let [values (into #{} (map pedestrian-graph (:networks config)))
            ag     (agent values :error-handler #(timbre/fatal %2 (deref %1))
                                 :error-mode :fail)]
        (assoc this :networks ag))))
  (stop [this]
    (timbre/info "stopping router")
    (assoc this :networks nil)))

(defn- stop-process
  [agnt error]
  (timbre/fatal error (deref agnt))
  (System/exit 1)) ;; stop program execution

(defrecord Router [config networks]
  component/Lifecycle
  (start [this]
    (if (not-empty (:networks this)) this
      (let [ag (agent #{} :error-handler stop-process
                          :error-mode :fail)]
        (doseq [area (:networks config)]
          (send-off ag #(time (conj %1 (network %2))) area))
        (assoc this :networks ag))))
  (stop [this]
    (timbre/info "stopping router")
    (assoc this :networks nil)))

(defn service
  "returns a Router record that will contain the config
   of and all the networks of the system as agents"
  [config]
  (if (:USE_FAKE_NETWORK config)
    (map->DevRouter {})
    (map->Router {})))
