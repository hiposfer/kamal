(ns hiposfer.kamal.services.routing.directions
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
  (:require [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.services.routing.transit :as transit]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.libs.fastq :as fastq]
            [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool])
  (:import (java.time LocalDateTime Duration LocalTime LocalDate)))

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

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- instruction
  "returns a human readable version of the maneuver to perform"
  [network result piece next-piece]
  (let [context  (transit/context network piece)
        modifier (:maneuver/modifier result)
        maneuver (:maneuver/type result)
        name     (transit/name context)]
    (case maneuver
      ;; walking normally
      "turn" ;; only applies for walking
      (str "Take " modifier (when name (str " on " name)))

      ;; taking a trip on a public transport vehicle
      "continue" ;; only applies to transit
      (let [tstart  (:start (val (first piece)))
            route   (:trip/route (:stop_times/trip tstart))
            vehicle (transit/route-types (:route/type route))
            id      (str vehicle " " (or (:route/short_name route)
                                         (:route/long_name route)))]
        (str "Continue on " id))

      "notification"
      (let [tend    (:start (val (first next-piece)))
            trip    (:stop_times/trip tend)
            route   (:trip/route trip)
            vehicle (transit/route-types (:route/type route))
            id      (str vehicle " " (or (:route/short_name route)
                                         (:route/long_name route)))]
        (str "Hop on " id " to " (:trip/headsign trip)))

      ;; exiting a vehicle
      "exit vehicle"
      (str "Exit the vehicle")

      ;; This is the first instruction that the user gets
      "depart"
      (let [n2 (transit/name (transit/context network next-piece))]
        (if (or name n2)
          (str "Head on to " (or name n2))
          "Depart"))

      ;; This is the last instruction that the user gets
      "arrive"
      "You have arrived at your destination")))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver-type
  [network prev-piece piece next-piece]
  (let [last-context (transit/context network prev-piece)
        context      (transit/context network piece)
        next-context (transit/context network next-piece)]
    (cond
      (= prev-piece piece) "depart"
      (= piece next-piece) "arrive"

      ;; change conditions, e.g. change of mode from walking to transit
      (and (not (transit/stop? last-context)) (transit/stop? context))
      "notification"

      ;; already on a transit trip, continue
      (and (transit/stop? context) (transit/stop? next-context))
      "continue"

      ;; change of conditions -> exit vehicle
      (and (transit/stop? context) (not (transit/stop? next-context)))
      "exit vehicle"

      :else                "turn")))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver ;; piece => [trace ...]
  "returns a step maneuver"
  [network prev-piece piece next-piece]
  (let [pre-bearing  (geometry/bearing (location (key (first prev-piece)))
                                       (location (key (first piece))))
        post-bearing (geometry/bearing (location (key (first piece)))
                                       (location (key (first next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        _type        (maneuver-type network prev-piece piece next-piece)
        result       (merge {:maneuver/bearing_before pre-bearing
                             :maneuver/bearing_after  post-bearing
                             :maneuver/type _type}
                       (when (= _type "turn")
                         {:maneuver/modifier (modifier angle _type)}))]
    (assoc result :maneuver/instruction (instruction network result piece next-piece))))

;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step ;; piece => [trace ...]
  "includes one StepManeuver object and travel to the following RouteStep"
  [^LocalDateTime start network prev-piece piece next-piece]
  (let [context (transit/context network piece)
        line    (linestring (map key (concat piece [(first next-piece)])))
        man     (maneuver network prev-piece piece next-piece)
        mode    (if (transit/stop? context) "transit" "walking")
        departs (np/cost (val (first piece)))
        arrives (np/cost (val (first next-piece)))]
    (merge man
      {:step/mode     mode
       :step/distance (geometry/arc-length (:coordinates line))
       :step/duration (Duration/ofSeconds (- arrives departs))
       :step/geometry line}
      (when (not-empty (transit/name context))
        {:step/name (transit/name context)})
      (if (= "arrive" (:type man))
        {:step/arrive (. start (plusSeconds arrives))}
        {:step/departure (. start (plusSeconds departs))})
      (when (= "transit" mode)
        (if (= "exit vehicle" (man :type))
          (tool/gtfs-resource (:end (val (first piece))))
          (tool/gtfs-resource (:start (val (first next-piece)))))))))

(defn- route-steps
  "returns a route-steps vector or an empty vector if no steps are needed"
  [network pieces midnight] ;; piece => [[trace via] ...]
  (let [start     [(first pieces)] ;; add depart and arrival pieces into the calculation
        end       (repeat 2 [(last (last pieces))]) ;; use only the last point as end - not the entire piece
        extended  (concat start pieces end)]
    (map step (repeat midnight)
              (repeat network)
              extended
              (rest extended)
              (rest (rest extended)))))

;https://www.mapbox.com/api-documentation/#route-object
(defn- route
  "a route from the first to the last waypoint. Only two waypoints
  are currently supported"
  [network rtrail midnight]
  (let [trail  (rseq (into [] rtrail))]
    (if (= (count trail) 1) ;; a single trace is returned for src = dst
      {:directions/distance 0 :directions/duration 0 :directions/steps []}
      (let [previous    (volatile! (first trail))
            pieces      (partition-by #(transit/name (transit/context! network % previous))
                                       trail)
            departs     (np/cost (val (first trail)))
            arrives     (np/cost (val (last trail)))]
        {:directions/distance (geometry/arc-length (:coordinates (linestring (map key trail))))
         :directions/duration (Duration/ofSeconds (- arrives departs))
         :directions/steps    (route-steps network pieces midnight)}))))

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
  (let [{:keys [coordinates ^LocalDateTime departure]} params
        date       (. departure (toLocalDate))
        trips      (fastq/day-trips network date)
        start      (Duration/between (LocalTime/MIDNIGHT)
                                     (. departure (toLocalTime)))
        src        (first (fastq/nearest-node network (first coordinates)))
        dst        (first (fastq/nearest-node network (last coordinates)))
       ; both start and dst should be found since we checked that before
        traversal  (alg/dijkstra network #{[src (. start (getSeconds))]}
                                 {:value-by   #(transit/timetable-duration network trips %1 %2)
                                  :successors transit/successors
                                  :comparator transit/by-cost})
        rtrail     (alg/shortest-path dst traversal)]
    (when (some? rtrail)
      (merge
        {:directions/uuid      (data/squuid)
         :directions/waypoints
         [{:waypoint/name     (some (comp not-empty :way/name)
                                    (fastq/node-ways network src))
           :waypoint/location (->coordinates (location src))}
          {:waypoint/name     (some (comp not-empty :way/name)
                                    (fastq/node-ways network dst))
           :waypoint/location (->coordinates (location dst))}]}
        (route network rtrail (. date (atStartOfDay)))))))

;(time
;  (direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;             {:coordinates [[8.645333, 50.087314]
;                            [8.635897, 50.104172]]
;              :departure (LocalDateTime/parse "2018-05-07T10:15:30")
;              :steps true}))
