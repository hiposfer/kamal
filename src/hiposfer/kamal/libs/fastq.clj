(ns hiposfer.kamal.libs.fastq
  "namespace for hand-optimized queries that are used inside the routing
  algorithm and need to run extremely fast (< 1 ms per query)

  By convention all queries here return Entities"
  (:require [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.string :as str])
  (:import (java.time LocalDate)))

(defn index-lookup
  "returns a transducer that can be used together with index-range to get all
  entities whose value equals id i.e. the entities that have a reference to id"
  [network id]
  (comp (take-while #(= (:v %) id))
        (map #(data/entity network (:e %)))))

(defn node-successors
  "takes a network and an entity and returns the successors of that entity.
   Only valid for OSM nodes. Assumes bidirectional links i.e. nodes with
   back-references to entity are also returned

  replaces:
  '[:find ?successors ?node
    :in $ ?id
    :where [?id :node/successors ?successors]
           [?node :node/successors ?id]]

  The previous query takes around 50 milliseconds to finish. This function
  takes around 0.25 milliseconds"
  [network entity]
  (let [id (:db/id entity)]
    (concat (:node/successors entity)
            (eduction (index-lookup network id)
                      (data/index-range network :node/successors id nil)))))


(defn nearest-node
  "returns the nearest node/location to point"
  [network point]
  (eduction (map :e)
            (map #(data/entity network %))
            (data/index-range network :node/location point nil)))

(defn node-ways
  "takes a dereferenced Datascript connection and an entity id and returns
  the OSM ways that reference it. Only valid for OSM node ids

  replaces:
  '[:find ?way
    :in $ ?id
    :where [?way :way/nodes ?id]]

  The previous query takes around 50 milliseconds to finish. This one takes
  around 0.15 milliseconds"
  [network entity]
  (let [id (:db/id entity)]
    (eduction (index-lookup network id)
              (data/index-range network :way/nodes id nil))))

(defn continue-trip
  "returns the :stop_time entity to reach ?dst-id via ?trip

  Returns nil if no trip going to ?dst-id was found

  replaces:
  '[:find ?departure
    :in $ ?dst-id ?trip ?start
    :where [?dst :stop_time/stop ?dst-id]
           [?dst :stop_time/trip ?trip]
           [?dst :stop_time/arrival_time ?seconds]
           [(plus-seconds ?start ?seconds) ?departure]]

   The previous query takes around 50 milliseconds to execute. This function
   takes around 0.22 milliseconds to execute. Depends on :stop_time/trip index"
  [network dst trip]
  (let [?dst-id  (:db/id dst)
        ?trip-id (:db/id trip)]
    (tool/some #(= ?dst-id (:db/id (:stop_time/stop %)))
               (eduction (index-lookup network ?trip-id)
                         (data/index-range network :stop_time/trip ?trip-id nil)))))

(defn find-trip
  "Returns a [src dst] :stop_time pair for the next trip between ?src-id
   and ?dst-id departing after ?now.

   Returns nil if no trip was found

  replaces:
  '[:find ?trip ?departure
    :in $ ?src-id ?dst-id ?now ?start
    :where [?src :stop_time/stop ?src-id]
           [?dst :stop_time/stop ?dst-id]
           [?src :stop_time/trip ?trip]
           [?dst :stop_time/trip ?trip]
           [?src :stop_time/departure_time ?amount]
           [(hiposfer.kamal.libs.fastq/plus-seconds ?start ?amount) ?departure]
           [(hiposfer.kamal.libs.fastq/after? ?departure ?now)]]

  The previous query runs in 118 milliseconds. This function takes 4 milliseconds"
  [network day-stops src dst now]
  (let [?src-id (:db/id src)
        stop_times (eduction (filter #(contains? day-stops (:e %)))
                             (index-lookup network ?src-id)
                             (filter #(> (:stop_time/departure_time %) now))
                             (data/index-range network :stop_time/stop ?src-id nil))]
    (when (not-empty stop_times)
      (let [trip (apply min-key :stop_time/departure_time stop_times)]
        [trip (continue-trip network dst (:stop_time/trip trip))]))))

(defn- working?
  [^LocalDate date service]
  (let [day  (str/lower-case (str (. date (getDayOfWeek))))
        work (keyword "service" day)] ;; :service/monday
    (pos? (work service)))) ;; 1 or 0 according to gtfs spec

(defn day-stop-times
  "returns a set of stop_times entity ids that are available for date"
  [network ^LocalDate date]
  (let [services (into #{} (comp (take-while #(= (:a %) :service/id))
                                 (map #(data/entity network (:e %)))
                                 (filter #(. date (isBefore (:service/end_date %))))
                                 (filter #(. date (isAfter (:service/start_date %))))
                                 (filter #(working? date %))
                                 (map :db/id))
                       (data/seek-datoms network :avet :service/id))
        trips    (into #{} (comp (take-while #(= (:a %) :trip/service))
                                 (filter #(contains? services (:v %)))
                                 (map :e))
                       (data/seek-datoms network :avet :trip/service))]
    (into #{} (comp (filter #(contains? trips (:v %)))
                    (map :e))
              (data/datoms network :avet :stop_time/trip))))

(defn- references
  "returns all entities that reference entity through attribute k"
  ([network k entity]
   (references network k entity (map identity)))
  ([network k entity xform]
   (eduction (comp (index-lookup network (:db/id entity))
                   xform)
             (data/index-range network k (:db/id entity) nil))))

;; This might not be the best approach but it gets the job done for the time being
(defn link-stops
  "takes a network, looks up the nearest node for each stop and returns
  a transaction that will link those"
  [network]
  (for [stop (map #(data/entity network (:e %))
                   (data/datoms network :aevt :stop/id))]
    (let [node (first (nearest-node network (:stop/location stop)))]
      (if (not (some? node))
        (throw (ex-info "stop didnt match to any known node in the OSM data"
                        (into {} stop)))
        {:node/id (:node/id node)
         :node/successors #{[:stop/id (:stop/id stop)]}}))))

(defn cache-stop-successors
  "computes the next-stops for each stop and returns a transaction
  that will cache those results inside the :stop entities"
  [network]
  (let [pairs (for [trip (data/datoms network :aevt :trip/id)
                    :let [entries (eduction (index-lookup network (:e trip))
                                            (data/index-range network :stop_time/trip (:e trip) nil))
                          stop_times (sort-by :stop_time/stop_sequence entries)]
                    pair (map vector stop_times (rest stop_times))]
                pair)]
    (for [[from to] (distinct pairs)]
      {:stop/id (:stop/id (:stop_time/stop from))
       :stop/successors #{[:stop/id (:stop/id (:stop_time/stop to))]}})))

;(time (last (data/with @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                       (cache-stop-successors @(first @(:networks (:router hiposfer.kamal.dev/system)))))))
