(ns hiposfer.kamal.router.directions
  "collection of functions to provide routing directions based on Open Street
  Maps and General Transit Feed Specification data. It follows (as close
  as possible) the MapBox v5 directions specification.

  The algorithm provides a single route between the first and last coordinate.
  It works as follows:
  - create a Dijkstra collection to find the shortest path
  - take the sequence and split it into 'pieces'
   - each piece is a subsection of the complete route with a common 'context'.
    The 'context' is the road name or the stop name
  - loop over the pieces to create instructions based on each previous, current
    and next piece"
  (:require [datascript.core :as data]
            [hiposfer.kamal.router.algorithms.dijkstra :as dijkstra]
            [hiposfer.kamal.router.algorithms.protocols :as np]
            [hiposfer.kamal.router.transit :as transit]
            [hiposfer.kamal.router.util.geometry :as geometry]
            [hiposfer.kamal.router.util.fastq :as fastq])
  (:import (java.time Duration LocalTime ZonedDateTime)
           (java.time.temporal ChronoUnit)))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(def bearing-turns
  (sorted-map 0    "straight"
              20   "slight right"
              60   "right"
              120  "sharp right"
              160  "uturn"
              200  "sharp left"
              240  "left"
              300  "slight left"
              340  "straight"))


(def ->coordinates (juxt np/lon np/lat))

(defn- location [e] (or (:node/location e) [(:stop/lon e) (:stop/lat e)]))

(defn- linestring
  "get a geojson linestring based on the route path"
  [entities]
  {:type "LineString"
   :coordinates (for [e entities] (->coordinates (location e)))})

(defn- modifier
  [angle _type]
  (case _type
    ("depart" "arrive") nil
    ;; return the turn indication based on the angle
    "turn") (val (last (subseq bearing-turns <= angle))))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- instruction
  "returns a human readable version of the maneuver to perform"
  [_type _modifier piece next-piece]
  (let [context (transit/context piece)
        _name   (transit/name context)]
    (case _type
      ;; walking normally
      "turn" ;; only applies for walking
      (str "Take " _modifier (when _name (str " on " _name)))

      ;; taking a trip on a public transport vehicle
      "continue" ;; only applies to transit
      (let [stoptime (:stop_time/to (val (first piece)))
            route    (:trip/route (:stop_time/trip stoptime))
            vehicle  (transit/route-types (:route/type route))
            id       (str vehicle " " (or (:route/short_name route)
                                          (:route/long_name route)))]
        (str "Continue on " id))

      "notification"
      (let [stoptime (:stop_time/to (val (first next-piece)))
            trip     (:stop_time/trip stoptime)
            route    (:trip/route trip)
            vehicle  (transit/route-types (:route/type route))
            id       (str vehicle " " (or (:route/short_name route)
                                          (:route/long_name route)))]
        (str "Hop on " id " to " (:trip/headsign trip)))

      ;; exiting a vehicle
      "exit vehicle"
      (str "Exit the vehicle")

      ;; This is the first instruction that the user gets
      "depart"
      (let [next-name (transit/name (transit/context next-piece))]
        (if (some? next-name)
          (str "Head on to " next-name)
          "Depart"))

      ;; This is the last instruction that the user gets
      "arrive"
      "You have arrived at your destination")))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver-type
  [prev-piece piece next-piece]
  (let [last-context (transit/context prev-piece)
        context      (transit/context piece)
        next-context (transit/context next-piece)]
    (cond
      (= prev-piece piece)
      "depart"

      (= piece next-piece)
      "arrive"

      ;; change conditions, e.g. change of mode from walking to transit
      (and (not (transit/stop? last-context)) (transit/stop? context))
      "notification"

      ;; already on a transit trip, continue
      (and (transit/stop? context) (transit/stop? next-context))
      "continue"

      ;; change of conditions -> exit vehicle
      (and (transit/stop? context) (not (transit/stop? next-context)))
      "exit vehicle"

      :else
      "turn")))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver ;; piece => [trace ...]
  "returns a step maneuver"
  [prev-piece piece next-piece]
  (let [pre-bearing  (geometry/bearing (location (key (first prev-piece)))
                                       (location (key (first piece))))
        post-bearing (geometry/bearing (location (key (first piece)))
                                       (location (key (first next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        _type        (maneuver-type prev-piece piece next-piece)
        _modifier    (modifier angle _type)
        human-text   (instruction _type _modifier piece next-piece)]
    (merge {:maneuver/bearing_before pre-bearing
            :maneuver/bearing_after  post-bearing
            :maneuver/type _type
            :maneuver/instruction human-text}
           (when (= _type "turn")
             {:maneuver/modifier _modifier}))))

;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step ;; piece => [trace ...]
  "includes one StepManeuver object and travel to the following RouteStep"
  [zone-midnight prev-piece piece next-piece]
  (let [context (transit/context piece)
        line    (linestring (map key (concat piece [(first next-piece)])))
        man     (maneuver prev-piece piece next-piece)
        mode    (if (transit/stop? context) "transit" "walking")
        departs (np/cost (val (first piece)))
        arrives (np/cost (val (first next-piece)))]
    (merge {:step/mode     mode
            :step/distance (geometry/arc-length (:coordinates line))
            :step/duration (- arrives departs)
            :step/geometry line
            :step/maneuver man}
           (when (not-empty (transit/name context))
             {:step/name (transit/name context)})
           (if (= "arrive" (:maneuver/type man))
             {:step/arrive (+ zone-midnight arrives)}
             {:step/departure (+ zone-midnight departs)})
           (when (= "transit" mode)
             {:step/trip (select-keys (:stop_time/trip (:stop_time/to (val (first next-piece))))
                                      [:trip/id])}))))

(defn- route-steps
  "returns a route-steps vector or an empty vector if no steps are needed"
  [pieces zone-midnight] ;; piece => [[trace via] ...]
  (let [start     [(first pieces)] ;; add depart and arrival pieces into the calculation
        end       [[(last (last pieces))]] ;; use only the last point as end - not the entire piece
        extended  (concat start pieces end)]
    (map step (repeat zone-midnight)
              extended
              (rest extended)
              (rest (rest extended)))))

;https://www.mapbox.com/api-documentation/#route-object
(defn- route
  "a route from the first to the last waypoint. Only two waypoints
  are currently supported"
  [trail zone-midnight]
  (if (= (count trail) 1) ;; a single trace is returned for src = dst
    {:directions/distance 0 :directions/duration 0 :directions/steps []}
    (let [previous    (volatile! (first trail))
          pieces      (partition-by #(transit/name (transit/context % previous))
                                     trail)
          departs     (np/cost (val (first trail)))
          arrives     (np/cost (val (last trail)))]
      {:directions/distance (geometry/arc-length (:coordinates (linestring (map key trail))))
       :directions/duration (- arrives departs)
       :directions/steps    (route-steps pieces zone-midnight)})))

;; for the time being we only care about the coordinates of start and end
;; but looking into the future it is good to make like this such that we
;; can always extend it with more parameters
;; https://www.mapbox.com/api-documentation/#retrieve-directions
(defn direction
  "given a network and a sequence of keywordized parameters according to
   https://www.mapbox.com/api-documentation/#retrieve-directions
   returns a directions object similar to the one from Mapbox directions API

   Example:
   (direction network :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]"
  [conn params]
  (let [{:keys [coordinates ^ZonedDateTime departure]} params
        graph   (get (meta conn) :area/graph)
        network (deref conn)
        trips   (fastq/day-trips network (. departure (toLocalDate)))
        init    (Duration/between (LocalTime/MIDNIGHT)
                                  (. departure (toLocalTime)))
        src     (first (fastq/nearest-nodes network (first coordinates)))
        dst     (first (fastq/nearest-nodes network (last coordinates)))]
    (when (and (some? src) (some? dst))
      (let [router (transit/->TransitRouter network graph trips)
            ; both start and dst should be found since we checked that before
            start  [(:db/id src) (. init (getSeconds))]
            path   (dijkstra/shortest-path router #{start} (:db/id dst))
            trail  (for [[id value] (reverse path)]
                     ;; HACK: prefetch the entity so that the rest of the code
                     ;; doesnt. TODO: figure out a better way to do this
                     (first {(data/entity network id) value}))]
        (when (not-empty trail)
          (merge {:directions/uuid      (data/squuid)
                  :directions/waypoints ;; use some for a best guess approach in case the first and last point dont have a way
                  [{:waypoint/name     (some (comp :way/name :way/entity val) trail)
                    :waypoint/location (->coordinates (location src))}
                   {:waypoint/name     (some (comp :way/name :way/entity val) (reverse trail))
                    :waypoint/location (->coordinates (location dst))}]}
                 (route trail (-> departure (.truncatedTo ChronoUnit/DAYS)
                                            (.toEpochSecond)))))))))

;(dotimes [n 1000]
#_(time (direction (first @(:networks (:router hiposfer.kamal.dev/system)))
                   {:coordinates [[8.645333, 50.087314]
                                  ;[8.680412, 50.116680]] ;; innenstadt
                                  ;[8.699619, 50.097842]] ;; sachsenhausen
                                  [8.635897, 50.104172]] ;; galluswarte
                    :departure (ZonedDateTime/parse "2018-05-07T10:15:30+02:00")}))
