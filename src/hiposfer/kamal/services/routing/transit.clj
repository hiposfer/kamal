(ns hiposfer.kamal.services.routing.transit
  "collection of functions related to the use of GTFS feed on routing
  networks."
  (:refer-clojure :exclude [name])
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data]
            [clojure.set :as set]
            [hiposfer.kamal.libs.tool :as tool]))

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
(defrecord TripStep [start end ^Long value]
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
    (stop? e) (:stop/name e)))

(defn- common-road
  "attempt to find the way that connect the passed arguments. Performs
  a best guess otherwise"
  ([network e1 e2]
   (let [s1 (fastq/node-ways network e1)
         s2 (fastq/node-ways network e2)
         ss (set/intersection (set s1) (set s2))]
     (tool/some :way/name ss)))
  ([network e1] ;; make the best guest
   (tool/some :way/name (fastq/node-ways network e1))))

(defn context!
  "Returns an entity holding the 'context' of the trace. For pedestrian routing
  that is the road name. For transit routing it is the stop name"
  [network trace vprev]
  (let [previous (key (deref vprev))
        current  (key trace)]
    (vreset! vprev trace)
    (cond
      ;; walking normally -> return the way
      (and (node? current) (node? previous))
      (common-road network current previous)
      ;; getting into a trip -> return the way of the road
      (and (node? previous) (stop? current))
      (common-road network previous);;  guessing here
      ;; on a trip -> return the stop
      (and (stop? current) (stop? previous))
      current
      ;; leaving a trip -> return the last stop
      (and (stop? previous) (node? current))
      previous)))

(defn context
  "utility function to give a common name to the repetitive execution of
  (key (first piece)) to retrieve the previously computed context

  Returns a stop or a way"
  [network piece] ;; a piece already represents a context ;)
  (let [e (key (first piece))]
    (if (stop? e) e ;; node otherwise
      (if (> 1 (count piece)) ;; TODO: can this be improved?
        (common-road network (key (first piece)) (key (second piece)))
        (common-road network (key (first piece)))))))

(defn successors
  "takes a network and an entity id and returns the successors of that entity"
  [network entity]
  (let [id           (:db/id entity)
        predecessors (eduction (fastq/index-lookup network id)
                               (data/index-range network :node/successors id nil))]
    (if (node? entity)
      (concat predecessors (:node/successors entity))
      (concat predecessors (:stop/successors entity)))))

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
      (let [[st1 st2] (fastq/find-trip network trips src dst now)]
        (when (some? st2) ;; nil if no trip was found
          ;(println {:src (:stop/name src)
          ;          :dst (:stop/name dst)
          ;          :route (:route/short_name (:trip/route (:stop.times/trip st1)))
          ;          :duration (- (:stop.times/arrival_time st2) now)})
          (->TripStep st1 st2 (- (:stop.times/arrival_time st2) now))))
      ;; the user is already in a trip. Just find the trip going to dst
      :else
      (let [?trip (:stop.times/trip (:start value))
            st    (fastq/continue-trip network dst ?trip)]
        (when (some? st)
          (->TripStep (:end value) st
                      (- (:stop.times/arrival_time st) now)))))))
