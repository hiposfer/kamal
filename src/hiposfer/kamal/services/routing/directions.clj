(ns hiposfer.kamal.services.routing.directions
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.libs.geometry :as geometry]
            [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.set :as set]))

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

(defn location
  "given an entity id returns its geo location assuming it is a node"
  [network id]
  (:node/location (data/entity network id)))

(defn- link
  "find the first link that connects src and dst and returns its entity id"
  [network src-trace dst-trace]
  (first (set/intersection (set (map :e (tool/node-ways network (key dst-trace))))
                           (set (map :e (tool/node-ways network (key src-trace)))))))

(defn duration
  "A very simple value computation function for arcs in a network.
  Returns the time it takes to go from src to dst based on walking speeds"
  [network next-id trail] ;; 1 is a simple value used for test whenever no other value would be suitable
  (let [src    (location network (key (first trail)))
        dst    (location network next-id)
        length (geometry/haversine src dst)]
    (/ length osm/walking-speed)))

(defn- linestring
  "get a geojson linestring based on the route path"
  [network ids]
  (let [coordinates (sequence (comp (map #(location network %))
                                    (map ->coordinates))
                              ids)]
    {:type "LineString"
     :coordinates coordinates}))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [network prev-piece  piece next-piece]
  (let [location     (location network (key (ffirst piece)))
        pre-bearing  (geometry/bearing (location network (key (ffirst prev-piece)))
                                       (location network (key (ffirst piece))))
        post-bearing (geometry/bearing (location network (key (ffirst piece)))
                                       (location network (key (ffirst next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        modifier     (val (last (subseq bearing-turns <= angle)))
        way-name     (:way/name (data/entity network (second (first piece))))
        instruction  (str "Take " modifier (when way-name (str " on " way-name)))]
    {:location (->coordinates  location)
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
    (let [way-ids     (map #(link network %1 %2) trail (rest trail))
          traces&ways (map vector trail (concat way-ids [(last way-ids)]))
          pieces      (partition-by #(:way/name (data/entity network (second %)))
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
  (let [{:keys [coordinates steps]} params
        start      (tool/nearest-node network (first coordinates)) ;; nil when lat,lon are both greater than
        dst        (tool/nearest-node network (last coordinates)) ;; any node in the network
       ; both start and dst should be found since we checked that before
        traversal  (alg/dijkstra network
                                 #(duration network %1 %2)
                                 tool/by-foot
                                 #{(:e start)})
        rtrail     (alg/shortest-path (:e dst) traversal)]
    (if (some? rtrail)
      {:code "Ok"
       :routes [(route network steps rtrail)]
       :waypoints [{:name (str (some :way/name (tool/node-ways network (:e start))))
                    :location (->coordinates (:v start))}
                   {:name (str (some :way/name (tool/node-ways network (:e dst))))
                    :location (->coordinates (:v dst))}]}
      {:code "NoRoute"
       :message "There was no route found for the given coordinates. Check for
                       impossible routes (e.g. routes over oceans without ferry connections)."})))
