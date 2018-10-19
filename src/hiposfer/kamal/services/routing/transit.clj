(ns hiposfer.kamal.services.routing.transit
  "collection of functions related to the use of GTFS feed on routing
  networks."
  (:refer-clojure :exclude [name])
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.io.osm :as osm]
            [hiposfer.kamal.services.routing.graph :as graph]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data]
            [clojure.set :as set]))

(defn node? [e] (boolean (:node/id e)))
(defn way? [e] (boolean (:way/id e)))
(defn stop? [e] (boolean (:stop/id e)))
;; ............................................................................
;; https://developers.google.com/transit/gtfs/reference/#routestxt
;; TODO: use gtfs.edn for this
(def route-types
  {0 "Tram"; Streetcar, Light rail. Any light rail or street level system within a metropolitan area.
   1 "Subway"; Metro. Any underground rail system within a metropolitan area.
   2 "Rail"; Used for intercity or long-distance travel.
   3 "Bus"; Used for short- and long-distance bus routes.
   4 "Ferry"; Used for short- and long-distance boat service.
   5 "Cable car"; Used for street-level cable cars where the cable runs beneath the car.
   6 "Gondola"; Suspended cable car. Typically used for aerial cable cars where the car is suspended from the cable.
   7 "Funicular"}); Any rail system designed for steep inclines.})

(defn walk-time
  ([p1 p2]
   (.longValue ^Double (/ (geometry/earth-distance p1 p2)
                          osm/walking-speed)))
  ([lon1 lat1 lon2 lat2]
   (.longValue ^Double (/ (geometry/earth-distance lon1 lat1 lon2 lat2)
                          osm/walking-speed))))

(def penalty 30) ;; seconds

;; ............................................................................
(defrecord WalkCost [^Long value]
  np/Valuable
  (cost [_] value)
  Comparable
  (compareTo [_ o] (Long/compare value (np/cost o))))

(defn walk-cost? [o] (instance? WalkCost o))

;; a TripStep represents the transition between two stops in a GTFS feed
;; Both the source and destination :stop_time are kept to avoid future lookups
(defrecord TripCost [^Long value]
  np/Valuable
  (cost [_] value)
  Comparable
  (compareTo [_ o] (Long/compare value (np/cost o))))

(defn trip-cost? [o] (instance? TripCost o))

;; ............................................................................
(defrecord TransitRouter [network graph trips]
  np/Dijkstra
  (node [this k] (get graph k))
  (relax [this arc trail]
    (let [[src-id value] (first trail)
          dst-id         (np/dst arc)
          src            (get graph src-id)
          dst            (get graph dst-id)
          now            (np/cost value)]
      #_(println "walk?:" (walk-cost? value) "trip?:" (trip-cost? value))
      (cond
        ;; The user is just walking so we route based on walking duration
        (graph/osm-node? src) ;; [node (or node stop)]
        (map->WalkCost {:value      (Long/sum now (walk-time src dst))
                        :way/entity (data/entity network (:way/e arc))})

        ;; the user is trying to leave a vehicle. Apply penalty but route
        ;; normally
        (graph/osm-node? dst) ;; [stop node]
        (map->WalkCost {:value      (Long/sum (Long/sum now penalty)
                                              (walk-time src dst))
                        :way/entity (data/entity network (:way/e arc))})

        ;; riding on public transport .............
        ;; the user is already in a trip. Just find the trip going to dst [stop stop]
        (trip-cost? value)
        (let [trip (:stop_time/trip (:stop_time/from value))
              st   (fastq/continue-trip network dst-id trip)]
          (when (some? st)
            (map->TripCost {:value          (:stop_time/arrival_time st)
                            :stop_time/from (:stop_time/to value)
                            :stop_time/to   st})))

        ;; the user is trying to get on a vehicle - find the next trip
        :else
        (let [route       (:route/e arc)
              route-trips (map :e (data/datoms network :avet :trip/route route))
              local-trips (set/intersection (set route-trips) trips)
              [st1 st2]   (fastq/find-trip network local-trips src-id dst-id now)]
          (when (some? st2) ;; nil if no trip was found
            (map->TripCost {:value          (:stop_time/arrival_time st2)
                            :stop_time/from st1
                            :stop_time/to   st2})))))))

(defn name
  "returns a name that represents this entity in the network.
   returns nil if the entity has no name"
  [e]
  (cond
    (way? e) (:way/name e)
    (stop? e) (:stop/name e)))

(defn context
  "Returns an entity holding the 'context' of the trace. For pedestrian routing
  that is the way. For transit routing it is the stop"
  ([piece] ;; a piece already represents a context ;)
   (let [step (val (first piece))]
     (if (walk-cost? step) ;; trip-step otherwise
       (:way/entity step)
       (key (first piece)))))
  ([trace vprev]
   (let [previous @vprev]
     (vreset! vprev trace)
     (cond
       ;; walking normally -> return the way
       (and (node? (key previous)) (node? (key trace)))
       (:way/entity (val trace))

       ;; getting into a trip -> return the way of the road
       (and (node? (key previous)) (stop? (key trace)))
       (:way/entity (val trace))

       ;; on a trip -> return the stop
       (and (stop? (key previous)) (stop? (key trace)))
       (key trace)

       ;; leaving a trip -> return the last stop
       (and (stop? (key previous)) (node? (key trace)))
       (key previous)))))
