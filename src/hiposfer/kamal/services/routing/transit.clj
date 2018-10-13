(ns hiposfer.kamal.services.routing.transit
  "collection of functions related to the use of GTFS feed on routing
  networks."
  (:refer-clojure :exclude [name])
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.io.osm :as osm]
            [hiposfer.kamal.libs.geometry :as geometry])
  (:import (datascript.impl.entity Entity)
           (clojure.lang IPersistentMap)))

(defn node? [e] (boolean (:node/id e)))
(defn way? [e] (boolean (:way/id e)))
(defn stop? [e] (boolean (:stop/id e)))

(extend-protocol np/Arc
  Entity
  (src [this] (or (:edge/src this) (:arc/src this)))
  (dst [this] (or (:edge/dst this) (:arc/dst this))))

(extend-protocol np/Bidirectional
  Entity
  (mirror [this] (merge (into {} this)
                        {:edge/src (:edge/dst this)
                         :edge/dst (:edge/src this)
                         :mirror   (not (:mirror this))}))
  (mirror? [this] false))

(extend-protocol np/Arc
  IPersistentMap
  (src [this] (:edge/src this))
  (dst [this] (:edge/dst this)))

(extend-protocol np/Bidirectional
  IPersistentMap
  (mirror [this] (assoc this :edge/src (:edge/dst this)
                             :edge/dst (:edge/src this)
                             :mirror   (not (:mirror this))))
  (mirror? [this] (boolean (:mirror this))))

(extend-protocol np/Node
  Entity
  (id [this] (:db/id this))
  (successors [this]
    (if (node? this)
      (fastq/node-edges this)
      (fastq/stop-successors this))))

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
   (Math/round ^Double (/ (geometry/earth-distance p1 p2)
                          osm/walking-speed)))
  ([lon1 lat1 lon2 lat2]
   (Math/round ^Double (/ (geometry/earth-distance lon1 lat1 lon2 lat2)
                          osm/walking-speed))))

(def penalty 30) ;; seconds

;; ............................................................................
(defrecord WalkStep [way ^Long value]
  np/Valuable
  (cost [_] value)
  Comparable
  (compareTo [_ o] (Long/compare value (np/cost o))))

(defn walk-step? [o] (instance? WalkStep o))

;; a TripStep represents the transition between two stops in a GTFS feed
;; Both the source and destination :stop_time are kept to avoid future lookups
(defrecord TripStep [start end ^Long value]
  np/Valuable
  (cost [_] value)
  Comparable
  (compareTo [_ o] (Long/compare value (np/cost o))))

(defn trip-step? [o] (instance? TripStep o))

;; ............................................................................
(defrecord StopTimesRouter [network day-stops]
  np/Router
  (relax [this arc trail]
    (let [dst         (np/dst arc)
          [src value] (first trail)
          now         (np/cost value)]
      (case [(node? src) (node? dst)]
        ;; The user just walking so we route based on walking duration
        [true true] ;; [node node]
        (->WalkStep (:edge/way arc)
                    (+ now (walk-time (:node/location src)
                                      (:node/location dst))))

        ;; The user is walking to a stop
        [true false] ;; [node stop]
        (let [location (:node/location src)]
          (->WalkStep (:edge/way arc) (+ now (walk-time (np/lon location)
                                                        (np/lat location)
                                                        (:stop/lon dst)
                                                        (:stop/lat dst)))))

        ;; the user is trying to leave a vehicle. Apply penalty but route
        ;; normally
        [false true] ;; [stop node]
        (let [location (:node/location dst)]
          (->WalkStep (:edge/way arc) (+ now penalty (walk-time (:stop/lon src)
                                                                (:stop/lat src)
                                                                (np/lon location)
                                                                (np/lat location)))))

        ;; riding on public transport
        [false false] ;; [stop stop]
        (if (trip-step? value)
          ;; the user is already in a trip. Just find the trip going to dst
          (let [?trip (:stop_time/trip (:start value))
                st    (fastq/continue-trip network dst ?trip)]
            (when (some? st)
              (->TripStep (:end value) st (:stop_time/arrival_time st))))

          ;; the user is trying to get on a vehicle - find the next trip
          (let [[st1 st2] (fastq/find-trip network day-stops src dst now)]
            (when (some? st2) ;; nil if no trip was found
              (->TripStep st1 st2 (:stop_time/arrival_time st2)))))))))

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
     (if (walk-step? step) ;; trip-step otherwise
       (:way step)
       (key (first piece)))))
  ([trace vprev]
   (let [previous @vprev]
     (vreset! vprev trace)
     (cond
       ;; walking normally -> return the way
       (and (node? (key previous)) (node? (key trace)))
       (:way (val previous))

       ;; getting into a trip -> return the way of the road
       (and (node? (key previous)) (stop? (key trace)))
       (:way (key previous))

       ;; on a trip -> return the stop
       (and (stop? (key previous)) (stop? (key trace)))
       (key previous)

       ;; leaving a trip -> return the last stop
       (and (stop? (key previous)) (node? (key trace)))
       (key previous)))))
