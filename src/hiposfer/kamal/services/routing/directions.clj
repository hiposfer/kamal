(ns hiposfer.kamal.services.routing.directions
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.set :as set]
            [hiposfer.kamal.libs.fastq :as fastq])
  (:import (java.time LocalDateTime)))

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
  "find the first link that connects src and dst and returns its entity id"
  [network src-trace dst-trace]
  (first (set/intersection (set (fastq/node-ways network (key dst-trace)))
                           (set (fastq/node-ways network (key src-trace))))))

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

(defn- by-cost
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
  (if (node? entity) ;; else stop entity
    (fastq/node-successors network entity)
    (concat (fastq/node-successors network entity)
            (fastq/node-successors network entity))))

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

(defn- linestring
  "get a geojson linestring based on the route path"
  [network entities]
  (let [coordinates (sequence (comp (map :node/location)
                                    (map ->coordinates))
                              entities)]
    {:type "LineString"
     :coordinates coordinates}))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [network prev-piece  piece next-piece]
  (let [position     (:node/location network (key (ffirst piece)))
        pre-bearing  (geometry/bearing (:node/location (key (ffirst prev-piece)))
                                       (:node/location (key (ffirst piece))))
        post-bearing (geometry/bearing (:node/location (key (ffirst piece)))
                                       (:node/location (key (ffirst next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        modifier     (val (last (subseq bearing-turns <= angle)))
        way-name     (:way/name (data/entity network (second (first piece))))
        instruction  (str "Take " modifier (when way-name (str " on " way-name)))]
    {:location (->coordinates  position)
     ;; todo: implement maneuver type
     ;https://www.mapbox.com/api-documentation/#maneuver-types
     :type     "turn"
     :bearing_before pre-bearing
     :bearing_after  post-bearing
     :modifier       modifier
     :instruction    instruction}))

;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step ;; piece => [[trace way] ...]
  "includes one StepManeuver object and travel to the following RouteStep"
  [network prev-piece piece next-piece]
  (let [linestring (linestring network (map (comp key first)
                                            (concat piece [(first next-piece)])))]
    {:distance (geometry/arc-length (:coordinates linestring))
     :duration (- (val (ffirst next-piece))
                  (val (ffirst piece)))
     :geometry linestring
     :name     (str (:way/name (data/entity network (second (first piece)))))
     :mode     "walking" ;;TODO this should not be hardcoded
     :maneuver (maneuver network prev-piece piece next-piece)
     :intersections []})) ;; TODO

(defn- route-steps
  "returns a route-steps vector or an empty vector if no steps are needed"
  [network steps pieces]
  (if (not steps) []
    (let [;; add depart and arrival pieces into the calculation
          pieces    (concat [(first pieces)] pieces [[(last (last pieces))]])
          routes    (map step (repeat network) pieces (rest pieces) (rest (rest pieces)))
          depart    (assoc-in (first routes) [:maneuver :type] "depart")
          orig-last (last (butlast pieces))
          arrive    (-> (step network orig-last (last pieces) (last pieces))
                        (assoc-in     [:maneuver :type] "arrive"))]
      (concat [depart] (rest routes) [arrive]))))

;https://www.mapbox.com/api-documentation/#routeleg-object
(defn- route-leg
  "a route between only two waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps trail]
  (if (= (count trail) 1) ;; a single trace is returned for src = dst
    {:distance 0 :duration 0 :steps [] :summary "" :annotation []}
    (let [ways        (map #(link network %1 %2) trail (rest trail))
          traces&ways (map vector trail (concat ways [(last ways)]))
          pieces      (partition-by #(:way/name (second %))
                                    traces&ways)]
      {:distance   (geometry/arc-length (:coordinates (linestring network (map key trail))))
       :duration   (np/cost (val (last trail)))
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
    {:geometry    (linestring network (map key trail))
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
  (let [{:keys [coordinates steps departure]} params
        src        (first (fastq/nearest-node network (first coordinates)))
        dst        (first (fastq/nearest-node network (last coordinates)))
       ; both start and dst should be found since we checked that before
        traversal  (alg/dijkstra network #{[src departure]}
                                 {:value-by   #(timetable-duration network %1 %2)
                                  :successors successors
                                  :comparator by-cost})
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
