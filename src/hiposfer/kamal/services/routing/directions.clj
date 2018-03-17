(ns hiposfer.kamal.services.routing.directions
  (:require [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.services.routing.transit :as transit]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data]
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
  "find the first link that connects src and dst and returns its entity id"
  [network src-trace dst-trace]
  (first (set/intersection (set (fastq/node-ways network (key dst-trace)))
                           (set (fastq/node-ways network (key src-trace))))))

(defn- location [e] (or (:node/location e) (:stop/location e)))

(defn- linestring
  "get a geojson linestring based on the route path"
  [network entities]
  (let [coordinates (sequence (comp (map location)
                                    (map ->coordinates))
                              entities)]
    {:type "LineString"
     :coordinates coordinates}))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [network prev-piece  piece next-piece]
  (let [position     (location (key (ffirst piece)))
        pre-bearing  (geometry/bearing (location (key (ffirst prev-piece)))
                                       (location (key (ffirst piece))))
        post-bearing (geometry/bearing (location (key (ffirst piece)))
                                       (location (key (ffirst next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        modifier     (val (last (subseq bearing-turns <= angle)))
        way-name     (:way/name (data/entity network (second (first piece))))
        instruction  (str "Take " modifier (when way-name (str " on " way-name)))]
    {:location (->coordinates position)
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
  (let [{:keys [coordinates steps ^LocalDateTime departure]} params
        date       (.toLocalDate departure)
        start      (Duration/between (LocalTime/MIDNIGHT)
                                     (.toLocalTime departure))
        ;; WARNING: this is just too slow :(
        ;network    (data/filter network #(transit/by-service-day %1 %2 date))
        src        (first (fastq/nearest-node network (first coordinates)))
        dst        (first (fastq/nearest-node network (last coordinates)))
       ; both start and dst should be found since we checked that before
        traversal  (alg/dijkstra network #{[src (.getSeconds start)]}
                                 {:value-by   #(transit/timetable-duration network %1 %2)
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
