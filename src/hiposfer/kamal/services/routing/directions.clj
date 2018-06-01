(ns hiposfer.kamal.services.routing.directions
  "collection of functions to provide routing directions based on Open Street
  Maps and General Transit Feed Specification data. It follows (as close)
  as possible the MapBox v5 directions specification.

  The algorithm provides a single route between the first and last coordinate.
  It works as follows:
  - create a Dijkstra collection to find the shortest path
  - take the sequence and split it into 'pieces'
   - each piece is a subsection of the complete route where the motion type
    is considered, the same. Such as the points in a street (with the same name)
    or a bus route
  - take each piece and wrap it together with the previous and next piece to
    provide context for human instructions
  - prepend a copy of the first piece and two of the last point of the last piece
    - we use the last point (instead of the complete piece) to have proper bearing
      for the last piece
  - create instructions based on each previous, current and next piece"
  (:require [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.services.routing.transit :as transit]
            [hiposfer.kamal.libs.geometry :as geometry]
            [clojure.set :as set]
            [hiposfer.kamal.libs.fastq :as fastq])
  (:import (java.time LocalDateTime Duration LocalTime)))

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

(defn- link
  "retuns a way or a stop.times entity that connects src and dst.
  Useful for spliting the trail into pieces according to the name
  provided by the connection"
  [network src-trace dst-trace]
  (let [src (key src-trace)
        dst (key dst-trace)]
    (cond
      ;; walking normally
      (and (transit/node? src) (transit/node? dst))
      (first (set/intersection (set (fastq/node-ways network src))
                               (set (fastq/node-ways network dst))))
      ;; getting into a trip
      (and (transit/node? src) (transit/stop? dst))
      (first (fastq/node-ways network src))
      ;; on a trip
      (and (transit/stop? src) (transit/stop? dst))
      (:start (val src-trace)) ;; TripStep
      ;; leaving a trip
      (and (transit/stop? src) (transit/node? dst))
      (first (fastq/node-ways network src)))))

(defn- location [e] (or (:node/location e) (:stop/location e)))

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

(defn- instruction
  "returns a human readable version of the maneuver to perform"
  [result prev-piece piece next-piece]
  (let [via      (transit/via piece)
        modifier (:modifier result)
        _type    (:type result)
        via-name (transit/name via)]
    (cond
      ;; walking normally
      (and (transit/way? via) (= _type "turn"))
      (str "Take " modifier (when via-name (str " on " via-name)))

      ;; taking a trip on a public transport vehicle
      (transit/stop-times? via)
      (let [trip  (:stop.times/trip via)
            route (:trip/route trip)
            vehicle (transit/route-types (:route/type route))
            id      (str vehicle " " (or (:route/short_name route)
                                         (:route/long_name route)))]
        (if (= _type "continue") ;; notification otherwise
          (str "Continue on " id)
          (str "Hop on " id " to " (:trip/headsign trip))))

      ;; Walking to a transit stop. We faked the connection so there is
      ;; no via name
      (and (nil? via) (transit/stop-times? (transit/via next-piece)))
      (str "Walk until " (transit/name (transit/via next-piece)))

      ;; exiting a vehicle
      (and (nil? via) (= _type "exit vehicle"))
      (str "Exit the vehicle") ;(:stop/name (key (ffirst piece)))
           ;(when (transit/via next-piece)
            ; (str ", and walk towards " (transit/name (transit/via next-piece))))
      ;; This is the first instruction that the user gets
      (= _type "depart")
      (if (nil? via-name) "Depart"
        (str "Head on to " via-name))
      ;; This is the last instruction that the user gets
      (= _type "arrive")
      "You have arrived at your destination")))

(defn- maneuver-type
  [prev-piece piece next-piece]
  (cond
    (= prev-piece piece) "depart"
    (= piece next-piece) "arrive"
    (and (transit/stop-times? (transit/via prev-piece))
         (transit/stop-times? (transit/via piece)))
    "continue" ;; already on a transit trip, continue
    (and (not (transit/stop-times? (transit/via prev-piece)))
         (transit/stop-times? (transit/via piece)))
    "notification" ;; change conditions, e.g. change of mode from walking to transit
    (and (transit/stop-times? (transit/via prev-piece))
         (not (transit/stop-times? (transit/via piece))))
    "exit vehicle"
    :else                "turn"))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver ;; piece => [[trace via] ...]
  "returns a step maneuver"
  [prev-piece  piece next-piece]
  (let [position     (location (key (ffirst piece)))
        pre-bearing  (geometry/bearing (location (key (ffirst prev-piece)))
                                       (location (key (ffirst piece))))
        post-bearing (geometry/bearing (location (key (ffirst piece)))
                                       (location (key (ffirst next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        _type        (maneuver-type prev-piece piece next-piece)
        _modifier    (modifier angle _type)
        result       {:location (->coordinates position)
                      :bearing_before pre-bearing
                      :bearing_after  post-bearing
                      :type _type}
        result        (if (not= _type "turn") result
                        (assoc result :modifier _modifier))
        text          (instruction result prev-piece piece next-piece)]
    (assoc result :instruction text)))


;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step ;; piece => [[trace via] ...]
  "includes one StepManeuver object and travel to the following RouteStep"
  [prev-piece piece next-piece]
  (let [line (linestring (map (comp key first)
                              (concat piece [(first next-piece)])))]
    {:distance (geometry/arc-length (:coordinates line))
     :duration (- (np/cost (val (ffirst next-piece)))
                  (np/cost (val (ffirst piece))))
     :geometry line
     :name     (str (transit/name (transit/via piece)))
     :mode     (if (transit/stop-times? (transit/via piece)) "transit" "walking")
     :maneuver (maneuver prev-piece piece next-piece)
     :intersections []})) ;; TODO

(defn- route-steps
  "returns a route-steps vector or an empty vector if no steps are needed"
  [steps pieces]
  (if (not steps) [] ;; piece => [[trace via] ...]
    (let [start     [(first pieces)] ;; add depart and arrival pieces into the calculation
          end       (repeat 2 [(last (last pieces))]) ;; use only the last point as end - not the entire piece
          extended  (concat start pieces end)]
      (map step extended (rest extended) (rest (rest extended))))))

;(defn- place-name
;  [network trace]
;  (let [src   (key trace)
;        value (val trace)]
;    (cond
;      ;; walking normally
;      (transit/node? (key trace))
;      (first (fastq/node-ways network src)) ;; guess the way where the user is
;      ;; getting into a trip
;      (and (transit/stop? src) (not (transit/trip-step? value)))
;      src
;      ;; on a trip
;      (and (transit/stop? src) (transit/stop? dst))
;      (:start (val trace)) ;; TripStep
;      ;; leaving a trip
;      (and (transit/stop? src) (transit/node? dst))
;      (first (fastq/node-ways network src))))
;  (cond
;    (way? e) (:way/name e)
;    (stop-times? e) (:stop/name (:stop.times/stop e))))

;https://www.mapbox.com/api-documentation/#routeleg-object
(defn- route-leg
  "a route between only two waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps trail];; trail => [trace ...]
  (if (= (count trail) 1) ;; a single trace is returned for src = dst
    {:distance 0 :duration 0 :steps [] :summary "" :annotation []}
    (let [vias        (map link (repeat network) trail (rest trail))
          traces&ways (map vector trail (concat vias [(last vias)]))
          pieces      (partition-by #(transit/name (second %)) traces&ways)]
      {:distance   (geometry/arc-length (:coordinates (linestring (map key trail))))
       :duration   (- (np/cost (val (last trail)))
                      (np/cost (val (first trail))))
       :steps      (route-steps steps pieces)
       :summary    "" ;; TODO
       :annotation []}))) ;; TODO

;https://www.mapbox.com/api-documentation/#route-object
(defn- route
  "a route through (potentially multiple) waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps rtrail]
  (let [trail  (rseq (into [] rtrail))
        leg    (route-leg network steps trail)
        trail  (if (= (count trail) 1) (repeat 2 (first trail)) trail)]
    {:geometry    (linestring (map key trail))
     :duration    (:duration leg)
     :distance    (:distance leg)
     :weight      (:duration leg)
     :weight_name "time"
     :legs        [leg]}))

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
  [network params]
  (let [{:keys [coordinates steps ^LocalDateTime departure]} params
        date       (.toLocalDate departure)
        trips      (fastq/day-trips network date)
        start      (Duration/between (LocalTime/MIDNIGHT)
                                     (.toLocalTime departure))
        src        (first (fastq/nearest-node network (first coordinates)))
        dst        (first (fastq/nearest-node network (last coordinates)))
       ; both start and dst should be found since we checked that before
        traversal  (alg/dijkstra network #{[src (.getSeconds start)]}
                                 {:value-by   #(transit/timetable-duration network trips %1 %2)
                                  :successors transit/successors
                                  :comparator transit/by-cost})
        rtrail     (alg/shortest-path dst traversal)]
    (if (some? rtrail)
      {:code "Ok"
       :routes [(route network steps rtrail)]
       :waypoints [{:name (str (some :way/name (fastq/node-ways network src)))
                    :location (->coordinates (:node/location src))}
                   {:name (str (some :way/name (fastq/node-ways network dst)))
                    :location (->coordinates (:node/location dst))}]}
      {:code "NoRoute"
       :message "There was no route found for the given coordinates. Check for
                       impossible routes (e.g. routes over oceans without ferry connections)."})))

;(time
;  (direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;             {:coordinates [[8.645333, 50.087314]
;                            [8.635897, 50.104172]]
;              :departure (LocalDateTime/parse "2018-05-07T10:15:30")
;              :steps true}))
