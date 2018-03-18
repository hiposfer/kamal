(ns hiposfer.kamal.services.routing.transit
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data])
  (:import (java.time LocalDate)))

(defn- walk-time [src dst]
  (/ (geometry/haversine (or (:node/location src)
                             (:stop/location src))
                         (or (:node/location dst)
                             (:stop/location dst)))
     osm/walking-speed))

(defn duration
  "A very simple value computation function for arcs in a network.
  Returns the time it takes to go from src to dst based on walking speeds"
  [network next-entity trail] ;; 1 is a simple value used for test whenever no other value would be suitable
  (walk-time (key (first trail)) next-entity))

;; a TripStep represents the transition between two stops in a GTFS feed
;; Both the source and destination stop.times are kept to avoid future lookups
(defrecord TripStep [source destination ^Long value]
  np/Valuable
  (cost [_] value)
  (sum [this that] (assoc this :value (+ value (np/cost that)))))
;; compare based on value not on trip. Useful for Fibonacci Heap order
;Comparable
;(compareTo [_ that] (compare value (np/cost that))))

(defn by-cost
  "comparator to avoid CastClassException due to Java's Long limited comparison"
  [a b]
  (.compareTo ^Long (np/cost a)
              ^Long (np/cost b)))

(def penalty 30) ;; seconds

(defn node? [e] (:node/id e))
(defn stop? [e] (:stop/id e))
(defn trip-step? [o] (instance? TripStep o))

(defn successors
  "takes a network and an entity id and returns the successors of that entity"
  [network entity]
  (let [sus (fastq/index-lookup network :node/successors (:db/id entity))]
    (if (node? entity)
      (concat sus (:node/successors entity))
      (concat sus (:stop/successors entity)))))

(defn timetable-duration
  "provides routing calculations using both GTFS feed and OSM nodes. Returns
  a Long for walking parts and a TripStep for GTFS related ones."
  [network dst trail]
  (let [[src value] (first trail)
        now         (np/cost value)]
    (cond
      ;; The user just walking so we route based on walking duration
      (node? src)
      (long (walk-time src dst))
      ;; the user is trying to leave a vehicle. Apply penalty but route
      ;; normally
      (and (stop? src) (node? dst))
      (+ penalty (long (walk-time src dst)))
      ;; the user is trying to get into a vehicle. We need to find the next
      ;; coming trip
      (and (stop? src) (stop? dst) (not (trip-step? value)))
      (let [[st1 st2] (fastq/find-trip network (:db/id src) (:db/id dst) now)]
        (when (some? st2)
          (->TripStep st1 st2 (- (:stop.times/arrival_time st2) now))))
      ;; the user is already in a trip. Just find that trip for the dst
      :else
      (let [st (fastq/continue-trip network (:db/id dst) (:db/id (:trip value)))]
        (when (some? st)
          (->TripStep (:destination value) st
                      (- (:stop.times/arrival_time st) (np/cost value))))))))


(defn by-service-day
  "filters the Database to contain remove stop.times entries that are not applicable
  to the current Date"
  [db datom ^LocalDate now]
  (if (not= :stop.times/stop (:a datom)) true
    (let [st      (data/entity db (:e datom))
          trip    (:stop.times/trip st)
          service (:trip/service trip)
          start   ^LocalDate (:service/start_date service)
          end     ^LocalDate (:service/end_date service)]
      (if (and (.isAfter now start) (.isBefore now end))
        (contains? (:service/days service) (.getDayOfWeek now))
        false))))
