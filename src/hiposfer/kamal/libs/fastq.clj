(ns hiposfer.kamal.libs.fastq
  "namespace for hand-optimized queries that are used inside the routing
  algorithm and need to run extremely fast (< 1 ms per query)

  By convention all queries here return Entities"
  (:require [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool])
  (:import (java.time Duration LocalTime LocalDate LocalDateTime)))

(defn index-lookup
  "returns a transducer that can be used together with index-range to get all
  entities attribute k equals id"
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
            (sequence (index-lookup network id)
                      (data/index-range network :node/successors id nil)))))


(defn nearest-node
  "returns the nearest node/location to point"
  [network point]
  (map #(data/entity network (:e %))
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
    (sequence (index-lookup network id)
              (data/index-range network :way/nodes id nil))))

(defn continue-trip
  "returns the :stop.times entity to reach ?dst-id via ?trip

  replaces:
  '[:find ?departure
    :in $ ?dst-id ?trip ?start
    :where [?dst :stop.times/stop ?dst-id]
           [?dst :stop.times/trip ?trip]
           [?dst :stop.times/arrival_time ?seconds]
           [(plus-seconds ?start ?seconds) ?departure]]

   The previous query takes around 50 milliseconds to execute. This function
   takes around 0.22 milliseconds to execute. Depends on :stop.times/trip index"
  [network ?dst-id ?trip]
  (tool/some #(= ?dst-id (:db/id (:stop.times/stop %)))
             (eduction (index-lookup network ?trip)
                       (data/index-range network :stop.times/trip ?trip nil))))

(defn- min-departure
  [result value]
  (if (> (:stop.times/departure_time result) (:stop.times/departure_time value))
    value
    result))

(defn find-trip
  "Returns a [src dst] stop.times pair for the next trip between ?src-id
   and ?dst-id departing after ?now

  replaces:
  '[:find ?trip ?departure
    :in $ ?src-id ?dst-id ?now ?start
    :where [?src :stop.times/stop ?src-id]
           [?dst :stop.times/stop ?dst-id]
           [?src :stop.times/trip ?trip]
           [?dst :stop.times/trip ?trip]
           [?src :stop.times/departure_time ?amount]
           [(hiposfer.kamal.libs.fastq/plus-seconds ?start ?amount) ?departure]
           [(hiposfer.kamal.libs.fastq/after? ?departure ?now)]]

  The previous query runs in 118 milliseconds. This function takes 4 milliseconds"
  [network trips ?src-id ?dst-id ?now]
  (let [sts  (seq (eduction (comp (index-lookup network ?src-id)
                                  (filter #(and (> (:stop.times/departure_time %) ?now)
                                                (contains? trips (:db/id (:stop.times/trip %))))))
                            (data/index-range network :stop.times/stop ?src-id nil)))]
    (when (seq sts)
      (let [trip (reduce min-departure sts)]
        [trip (continue-trip network ?dst-id (:db/id (:stop.times/trip trip)))]))))

;(time
;  (dotimes [n 1000]
;    (find-trip @(first @(:networks (:router hiposfer.kamal.dev/system)))
;               230963
;               230607
;               (.getSeconds (Duration/between (LocalTime/MIDNIGHT)
;                                              (LocalTime/now)))))

(defn day-trips
  "returns a set of trip (entities) ids that are available for date"
  [network ^LocalDate date]
  (let [services (into #{} (comp (take-while #(= (:a %) :service/id))
                                 (map #(data/entity network (:e %)))
                                 (filter #(and (.isBefore date (:service/end_date %))
                                               (.isAfter date (:service/start_date %))
                                               (contains? (:service/days %)
                                                          (.getDayOfWeek date))))
                                 (map :db/id))
                           (data/seek-datoms network :avet :service/id))]
    (into #{} (comp (take-while #(= (:a %) :trip/service))
                    (filter #(contains? services (:v %)))
                    (map :e))
              (data/seek-datoms network :avet :trip/service))))

(defn next-stops
  "return the next stop entities for ?src-id based on stop.times

  replaces:
  '[:find [?id ...]
    :in $ ?src-id
    :where [?src :stop.times/stop ?src-id]
           [?src :stop.times/trip ?trip]
           [?dst :stop.times/trip ?trip]
           [?src :stop.times/sequence ?s1]
           [?dst :stop.times/sequence ?s2]
           [(> ?s2 ?s1)]
           [?dst :stop.times/stop ?se]
           [?se :stop/id ?id]]
  the previous query takes 145 milliseconds. This function takes 0.2 milliseconds"
  [network ?src]
  (let [id         (:db/id ?src)
        stop-times (sequence (index-lookup network id)
                             (data/index-range network :stop.times/stop id nil))
        stops      (for [st1  stop-times
                         :let [trip-id (:db/id (:stop.times/trip st1))
                               stimes2 (sequence (index-lookup network trip-id)
                                                 (data/index-range network :stop.times/trip trip-id nil))]
                         st2  stimes2
                         :when (> (:stop.times/sequence st2) (:stop.times/sequence st1))]
                     (:stop.times/stop st2))]
    (distinct stops)))

;(time
;  (dotimes [n 10000]
;    (next-stops @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                (data/entity @(first @(:networks (:router hiposfer.kamal.dev/system))) 230963))))
