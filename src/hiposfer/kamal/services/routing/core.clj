(ns hiposfer.kamal.services.routing.core
  (:require [datascript.core :as data]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.io.edn :as edn]
            [hiposfer.kamal.io.gtfs :as gtfs]
            [hiposfer.gtfs.edn :as gtfs.edn]))

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
                        {:area/id       {:db.unique :db.unique/identity}
                         ;; Open Street Map - entities
                         :node/id       {:db.unique :db.unique/identity}
                         :node/location {:db/index true}
                         ;; bidirecitonal edges
                         :node/edges    {:db.type        :db.type/ref
                                         :db.cardinality :db.cardinality/many}
                         :arc/way       {:db.type        :db.type/ref
                                         :db.cardinality :db.cardinality/one}
                         :arc/route     {:db.type        :db.type/ref
                                         :db.cardinality :db.cardinality/one}
                         :arc/dst       {:db.type        :db.type/ref
                                         :db.cardinality :db.cardinality/one}
                         :way/id        {:db.unique :db.unique/identity}}
                        ;; General Transfer Feed Specification - entities
                        ;; identities
                        (into {}
                          (for [id gtfs/uniques]
                            [id {:db.unique :db.unique/identity}]))
                        ;; references
                        (into {}
                          (for [f gtfs.edn/fields :when (gtfs/ref? f)]
                            [(f :keyword) {:db/type :db.type/ref}]))
                        ;; custom extensions
                        ;; unidirectional arcs
                        {:stop/arcs {:db.type        :db.type/ref
                                     :db.cardinality :db.cardinality/many}}))

(defn network
  "builds a datascript in-memory db and conj's it into the passed agent"
  [area]
  ;; re-build the network from the file
  (data/conn-from-db (edn/parse (:area/edn area))))

(defn- stop-process
  [agnt error]
  (binding [*out* *err*]
    (println error (deref agnt)))
  (System/exit 1)) ;; stop program execution

(defrecord AreaContainer [config networks]
  component/Lifecycle
  (start [this]
    (if (some? (:networks this)) this
      (let [ag (agent #{} :error-handler stop-process
                          :error-mode :fail)]
        (doseq [area (:networks config)]
          (println "starting area router:" area)
          (send ag #(time (conj %1 (network %2))) area))
        (assoc this :networks ag))))
  (stop [this]
    (println "stopping router")
    (assoc this :networks nil)))

(defn service
  "returns a Router record that will contain the config
   of and all the networks of the system as agents"
  []
  (map->AreaContainer {}))
