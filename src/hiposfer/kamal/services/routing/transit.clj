(ns hiposfer.kamal.services.routing.transit
  (:refer-clojure :exclude [name])
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data]))

;; https://developers.google.com/transit/gtfs/reference/#routestxt
(def route-types
  {0 "Tram"; Streetcar, Light rail. Any light rail or street level system within a metropolitan area.
   1 "Subway"; Metro. Any underground rail system within a metropolitan area.
   2 "Rail"; Used for intercity or long-distance travel.
   3 "Bus"; Used for short- and long-distance bus routes.
   4 "Ferry"; Used for short- and long-distance boat service.
   5 "Cable car"; Used for street-level cable cars where the cable runs beneath the car.
   6 "Gondola"; Suspended cable car. Typically used for aerial cable cars where the car is suspended from the cable.
   7 "Funicular"}); Any rail system designed for steep inclines.})

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
(defn way? [e] (:way/id e))
(defn stop? [e] (:stop/id e))
(defn stop-times? [e] (:stop.times/trip e))
(defn trip-step? [o] (instance? TripStep o))

(defn name
  "returns a name that represents this entity in the network.
   returns nil if the entity has no name"
  [e]
  (cond
    (way? e) (:way/name e)
    (stop-times? e) (:trip/headsign (:stop.times/trip e))))

(defn successors
  "takes a network and an entity id and returns the successors of that entity"
  [network entity]
  (let [id  (:db/id entity)
        sus (eduction (fastq/index-lookup network id)
                      (data/index-range network :node/successors id nil))]
    (if (node? entity)
      (concat sus (:node/successors entity))
      (concat sus (:stop/successors entity)))))

(defn timetable-duration
  "provides routing calculations using both GTFS feed and OSM nodes. Returns
  a Long for walking parts and a TripStep for GTFS related ones."
  [network trips dst trail]
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
      (let [[st1 st2] (fastq/find-trip network trips (:db/id src) (:db/id dst) now)]
        (when (some? st2)
          (->TripStep st1 st2 (- (:stop.times/arrival_time st2) now))))
      ;; the user is already in a trip. Just find that trip for the dst
      :else
      (let [st (fastq/continue-trip network (:db/id dst) (:db/id (:trip value)))]
        (when (some? st)
          (->TripStep (:destination value) st
                      (- (:stop.times/arrival_time st) (np/cost value))))))))
