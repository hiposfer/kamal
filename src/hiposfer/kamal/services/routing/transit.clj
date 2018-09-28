(ns hiposfer.kamal.services.routing.transit
  "collection of functions related to the use of GTFS feed on routing
  networks."
  (:refer-clojure :exclude [name])
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.libs.geometry :as geometry]
            [clojure.set :as set]
            [hiposfer.kamal.libs.tool :as tool]
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

(defn- walk-time
  ([p1 p2]
   (/ (geometry/earth-distance p1 p2)
      osm/walking-speed))
  ([lon1 lat1 lon2 lat2]
   (/ (geometry/earth-distance lon1 lat1 lon2 lat2)
      osm/walking-speed)))

(defn duration
  "A very simple value computation function for arcs in a network.
  Returns the time it takes to go from src to dst based on walking speeds"
  [network next-entity trail] ;; 1 is a simple value used for test whenever no other value would be suitable
  (walk-time (:node/location (key (first trail)))
             (or (:node/location next-entity)
                 [(:stop/lon next-entity) (:stop/lat next-entity)])))

(defn by-cost
  "comparator to avoid CastClassException due to Java's Long limited comparison"
  [a b]
  (.compareTo ^Long (np/cost a)
              ^Long (np/cost b)))

(def penalty 30) ;; seconds

(defn node? [e] (boolean (:node/id e)))
(defn way? [e] (boolean (:way/id e)))
(defn stop? [e] (boolean (:stop/id e)))

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
  [e1 e2]
  (let [s1 (:node/ways e1)
        s2 (:node/ways e2)
        ss (set/intersection (set s1) (set s2))]
    (tool/some :way/name ss)))

(defn context
  "Returns an entity holding the 'context' of the trace. For pedestrian routing
  that is the road name. For transit routing it is the stop name"
  ([piece] ;; a piece already represents a context ;)
   (let [e (key (first piece))]
     (cond
       (stop? e) e ;; node otherwise
       ;; TODO: can this be improved?
       (> 1 (count piece)) (common-road (key (first piece))
                                        (key (second piece)))
       ;;  guessing here
       :else (tool/some :way/name (:node/ways (key (first piece)))))))
  ([trace vprev]
   (let [previous (key (deref vprev))
         current  (key trace)]
     (vreset! vprev trace)
     (cond
       ;; walking normally -> return the way
       (and (node? previous) (node? current))
       (common-road previous current)
       ;; getting into a trip -> return the way of the road
       (and (node? previous) (stop? current))
       (tool/some :way/name (:node/ways previous));;  guessing here
       ;; on a trip -> return the stop
       (and (stop? previous) (stop? current))
       current
       ;; leaving a trip -> return the last stop
       (and (stop? previous) (node? current))
       previous))))

;; a TripStep represents the transition between two stops in a GTFS feed
;; Both the source and destination :stop_time are kept to avoid future lookups
(defrecord TripStep [start end ^Long value]
  np/Valuable
  (cost [_] value)
  (sum [this that] (assoc this :value (+ value (np/cost that)))))

(defn trip-step? [o] (instance? TripStep o))

(defrecord StopTimesRouter [network day-stops]
  np/Router
  (weight [this dst trail]
    (let [[src value] (first trail)
          now         (np/cost value)]
      (case [(node? src) (node? dst)]
        ;; The user just walking so we route based on walking duration
        [true true] ;; [node node]
        (long (walk-time (:node/location src) (:node/location dst)))

        ;; The user is walking to a stop
        [true false] ;; [node stop]
        (let [location (:node/location src)]
          (long (walk-time (np/lon location)
                           (np/lat location)
                           (:stop/lon dst)
                           (:stop/lat dst))))

        ;; the user is trying to leave a vehicle. Apply penalty but route
        ;; normally
        [false true] ; [stop node]
        (let [location (:node/location dst)]
          (+ penalty (long (walk-time (:stop/lon src)
                                      (:stop/lat src)
                                      (np/lon location)
                                      (np/lat location)))))

        ;; riding on public transport - [stop stop]
        [false false]
        (if (trip-step? value)
          ;; the user is already in a trip. Just find the trip going to dst
          (let [?trip (:stop_time/trip (:start value))
                st    (fastq/continue-trip network dst ?trip)]
            (when (some? st)
              (->TripStep (:end value) st
                          (- (:stop_time/arrival_time st) now))))

          ;; the user is trying to get on a vehicle - find the next trip
          (let [[st1 st2] (fastq/find-trip network day-stops src dst now)]
            (when (some? st2) ;; nil if no trip was found
              (->TripStep st1 st2 (- (:stop_time/arrival_time st2) now))))))))
  (successors [this entity]
    (let [id (:db/id entity)
          predecesors (eduction (fastq/index-lookup network id)
                                (data/index-range network :node/successors id nil))]

      (if (node? entity)
        (concat predecesors (:node/successors entity))
        (concat predecesors (:stop/successors entity))))))
