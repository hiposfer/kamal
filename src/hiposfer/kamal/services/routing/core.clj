(ns hiposfer.kamal.services.routing.core
  (:require [hiposfer.kamal.network.generators :as ng]
            [clojure.test.check.generators :as gen]
            [datascript.core :as data]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [taoensso.timbre :as timbre]
            [hiposfer.kamal.parsers.edn :as edn]
            [hiposfer.kamal.parsers.gtfs.core :as gtfs]
            [hiposfer.kamal.parsers.gtfs.reference :as reference]
            [clojure.data]))

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

(def schema (merge-with into
              {:area/name       {:db.unique :db.unique/identity}
               ;; Open Street Map - entities
               :node/id         {:db.unique :db.unique/identity}
               :node/location   {:db/index true}
               :node/successors {:db.type        :db.type/ref
                                 :db.cardinality :db.cardinality/many
                                 :db/index       true}

               :way/id          {:db.unique :db.unique/identity}
               :way/nodes       {:db.type        :db.type/ref
                                 :db.cardinality :db.cardinality/many
                                 :db/index       true}}
               ;; General Transfer Feed Specification - entities
               ;; identities
              (into {}
                (for [id gtfs/idents]
                  [id {:db.unique :db.unique/identity}]))
              ;; references
              (into {}
                (for [f reference/fields :when (gtfs/ref? f)]
                  [(f :keyword) {:db/type :db.type/ref}]))
              ;; custom extensions
              {:stop/successors {:db.type        :db.type/ref
                                 :db.cardinality :db.cardinality/many}
               ;; to allow index-range usage
               :stop/location   {:db/index true}
               :stop_time/trip {:db/index true}
               :stop_time/stop {:db/index true}}))

(defn network
  "builds a datascript in-memory db and conj's it into the passed agent"
  [area]
  ;; re-build the network from the file
  (data/conn-from-db (edn/parse (:area/edn area))))

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
          (timbre/info "starting area router:" area)
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


;(clojure.data/diff schema schema2)
;=>
;({:service/id {:db.unique :db.unique/identity},
;  :stop_times/stop #:db{:type :db.type/ref, :index true},
;  :stop_times/trip #:db{:type :db.type/ref, :index true}
;  {:fare_rule/route #:db{:type :db.type/ref},
;   :frequencie/trip #:db{:type :db.type/ref},
;   :fare_attribute/id {:db.unique :db.unique/identity},
;   :trip/shape #:db{:type :db.type/ref},
;   :shape/id {:db.unique :db.unique/identity},
;   :fare_attribute/agency #:db{:type :db.type/ref},
;   :stop_time/trip #:db{:type :db.type/ref, :index true},
;   :fare_rule/fare #:db{:type :db.type/ref},
;   :calendar/id {:db.unique :db.unique/identity},
;   :calendar_date/service #:db{:type :db.type/ref},
;   :stop_time/stop #:db{:type :db.type/ref, :index true}}
;  {:way/nodes {:db.type :db.type/ref, :db.cardinality :db.cardinality/many, :db/index true},
;   :way/id {:db.unique :db.unique/identity},
;   :node/successors {:db.type :db.type/ref, :db.cardinality :db.cardinality/many, :db/index true},
;   :stop/successors {:db.type :db.type/ref, :db.cardinality :db.cardinality/many},
;   :trip/route #:db{:type :db.type/ref},
;   :route/id {:db.unique :db.unique/identity},
;   :trip/id {:db.unique :db.unique/identity},
;   :agency/id {:db.unique :db.unique/identity},
;   :node/id {:db.unique :db.unique/identity},
;   :stop/id {:db.unique :db.unique/identity},
;   :route/agency #:db{:type :db.type/ref},
;   :stop/location #:db{:index true},
;   :node/location #:db{:index true},
;   :area/name {:db.unique :db.unique/identity},
;   :trip/service #:db{:type :db.type/ref}}})