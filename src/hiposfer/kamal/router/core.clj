(ns hiposfer.kamal.router.core
  (:require [datascript.core :as data]
            [com.stuartsierra.component :as component]
            [hiposfer.gtfs.edn :as gtfs.edn]
            [hiposfer.kamal.router.io.edn :as edn]
            [hiposfer.kamal.router.io.gtfs :as gtfs]
            [hiposfer.kamal.router.graph :as graph]))

;; NOTE: we use a node/location instead of lat, lon separate to avoid creating
;; so many datoms and to have a single entity that we can apply protocols against.
;; This is a hack !! but it works :)
;; Thanks datascript

(def schema (merge-with into
              {:area/id       {:db.unique :db.unique/identity}
               ;; Open Street Map - entities
               :node/id       {:db.unique :db.unique/identity}
               :node/location {:db/index true}
               ;; bidirectional links
               :edge/src       {:db.type        :db.type/ref
                                :db.cardinality :db.cardinality/one}
               :edge/dst       {:db.type        :db.type/ref
                                :db.cardinality :db.cardinality/one}
               :edge/way       {:db.type        :db.type/ref
                                :db.cardinality :db.cardinality/one}
               ;; unidirectional links
               :arc/src       {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/one}
               :arc/dst       {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/one}
               :arc/route     {:db.type        :db.type/ref
                               :db.cardinality :db.cardinality/one}
               :way/id        {:db.unique :db.unique/identity}}
              ;; General Transfer Feed Specification - entities
              ;; identities
              (into {}
                (for [id gtfs/uniques]
                  [id {:db.unique :db.unique/identity}]))
              ;; references
              (into {}
                (for [f (gtfs.edn/fields (gtfs.edn/spec))
                      :when (gtfs/ref? f)]
                  [(f :keyword) {:db/type :db.type/ref}]))))

(defn network
  "builds a datascript in-memory db and conj's it into the passed agent"
  [area]
  ;; re-build the network from the file
  (let [db   (edn/parse (:area/edn area))
        conn (data/conn-from-db db)]
    (alter-meta! conn assoc :area/graph (time (graph/create db)))
    conn))

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
