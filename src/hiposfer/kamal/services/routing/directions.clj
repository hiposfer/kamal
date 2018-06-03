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
        modifier (:modifier result)
        maneuver (:type result)
        name     (transit/name context)]
    (case maneuver
      ;; walking normally
      "turn" ;; only applies for walking
      (str "Take " modifier (when name (str " on " name)))

      ;; taking a trip on a public transport vehicle
      "continue" ;; only applies to transit
      (let [tstart  (:start (val (first piece)))
            route   (:trip/route (:stop.times/trip tstart))
            vehicle (transit/route-types (:route/type route))
            id      (str vehicle " " (or (:route/short_name route)
                                         (:route/long_name route)))]
        (str "Continue on " id))

      "notification"
      (let [tend    (:start (val (first next-piece)))
            trip    (:stop.times/trip tend)
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
  [network prev-piece  piece next-piece]
  (let [position     (location (key (first piece)))
        pre-bearing  (geometry/bearing (location (key (first prev-piece)))
                                       (location (key (first piece))))
        post-bearing (geometry/bearing (location (key (first piece)))
                                       (location (key (first next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        _type        (maneuver-type network prev-piece piece next-piece)
        _modifier    (modifier angle _type)
        result       {:location (->coordinates position)
                      :bearing_before pre-bearing
                      :bearing_after  post-bearing
                      :type _type}
        result        (if (not= _type "turn") result
                        (assoc result :modifier _modifier))
        text          (instruction network result piece next-piece)]
    (assoc result :instruction text)))


;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step ;; piece => [trace ...]
  "includes one StepManeuver object and travel to the following RouteStep"
  [network prev-piece piece next-piece]
  (let [context (transit/context network piece)
        line    (linestring (map key (concat piece [(first next-piece)])))
        man     (maneuver network prev-piece piece next-piece)]
    {:distance (geometry/arc-length (:coordinates line))
     :duration (- (np/cost (val (first next-piece)))
                  (np/cost (val (first piece))))
     :geometry line
     :name     (str (transit/name context))
     :mode     (if (transit/stop? context) "transit" "walking")
     :maneuver man
     :intersections []})) ;; TODO

(defn- route-steps
  "returns a route-steps vector or an empty vector if no steps are needed"
  [network steps pieces]
  (if (not steps) [] ;; piece => [[trace via] ...]
    (let [start     [(first pieces)] ;; add depart and arrival pieces into the calculation
          end       (repeat 2 [(last (last pieces))]) ;; use only the last point as end - not the entire piece
          extended  (concat start pieces end)]
      (map step (repeat network) extended (rest extended) (rest (rest extended))))))

;https://www.mapbox.com/api-documentation/#routeleg-object
(defn- route-leg
  "a route between only two waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps trail];; trail => [trace ...]
  (if (= (count trail) 1) ;; a single trace is returned for src = dst
    {:distance 0 :duration 0 :steps [] :summary "" :annotation []}
    (let [previous    (volatile! (first trail))
          pieces      (partition-by #(transit/name (transit/context! network % previous))
                                     trail)]
      {:distance   (geometry/arc-length (:coordinates (linestring (map key trail))))
       :duration   (- (np/cost (val (last trail)))
                      (np/cost (val (first trail))))
       :steps      (route-steps network steps pieces)
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
