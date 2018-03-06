(ns hiposfer.kamal.services.routing.directions
  (:require [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.network.graph.protocols :as gp]
            [hiposfer.kamal.libs.geometry :as geometry]))

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

;; WARNING: we assume that we only traverse outgoing arcs
;; and that there is only one arc connecting src & dst
(defn- link
  "find the link that connects src and dst and returns it"
  [graph src-trace dst-trace]
  (some #(when (= (key dst-trace) (gp/dst %)) %)
        (gp/successors (graph (key src-trace)))))


(defn duration
  "A very simple value computation function for Arcs in a graph.
  Returns the time it takes to go from arc src to dst based on osm/speeds"
  [graph arc _] ;; 1 is a simple value used for test whenever no other value would be suitable
  (let [src    (graph (gp/src arc))
        dst    (graph (gp/dst arc))
        length (geometry/haversine (np/lon src) (np/lat src)
                                   (np/lon dst) (np/lat dst))]
    (/ length osm/walking-speed)))

(defn- linestring
  "get a geojson linestring based on the route path"
  [{:keys [graph]} ids]
  (let [coordinates (sequence (comp (map graph)
                                    (map ->coordinates))
                              ids)]
    ;; ids is in reverse order so we need to order it
    {:type "LineString"
     :coordinates coordinates}))

;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [{:keys [graph ways]} prev-piece  piece next-piece]
  (let [location     (->coordinates (graph (key (ffirst piece))))
        pre-bearing  (geometry/bearing (graph (key (ffirst prev-piece)))
                                       (graph (key (ffirst piece))))
        post-bearing (geometry/bearing (graph (key (ffirst piece)))
                                       (graph (key (ffirst next-piece))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        modifier     (val (last (subseq bearing-turns <= angle)))
        way-name     (:name (ways (second (first piece))))
        instruction  (str "Take " modifier (when way-name (str " on " way-name)))]
    {:location location
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
  [{:keys [ways] :as network} prev-piece piece next-piece]
  (let [linestring (linestring network (map (comp key first)
                                            (concat piece [(first next-piece)])))]
    {:distance (geometry/arc-length (:coordinates linestring))
     :duration (- (val (ffirst next-piece))
                  (val (ffirst piece)))
     :geometry linestring
     :name     (str (:name (ways (second (first piece)))))
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
  [{:keys [graph ways] :as network} steps trail]
  (if (= (count trail) 1) ;; a single trace is returned for src = dst
    {:distance 0 :duration 0 :steps [] :summary "" :annotation []}
    (let [way-ids     (sequence (comp (map (partial link graph))
                                      (map np/way))
                                trail (rest trail))
          ways&traces (map vector trail (concat way-ids [(last way-ids)]))
          pieces      (partition-by #(:name (ways (second %))) ways&traces)]
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
  [{:keys [graph ways neighbours] :as network} params]
  (let [{:keys [coordinates steps]} params
        start     (avl/nearest neighbours >= (first coordinates)) ;; nil when lat,lon are both greater than
        dst       (avl/nearest neighbours >= (last coordinates)) ;; any node in the network
       ; both start and dst should be found since we checked that before
        traversal  (alg/dijkstra (:graph network)
                                 #(duration graph %1 %2)
                                 #{(val start)})
        rtrail     (alg/shortest-path (val dst) traversal)]
    (if (some? rtrail)
      {:code "Ok"
       :routes [(route network steps rtrail)]
       :waypoints [{:name (str (:name (ways (some np/way (gp/successors (key start))))))
                    :location (->coordinates (key start))}
                   {:name (str (:name (ways (some np/way (gp/successors (key dst))))))
                    :location (->coordinates (key dst))}]}
      {:code "NoRoute"
       :message "There was no route found for the given coordinates. Check for
                       impossible routes (e.g. routes over oceans without ferry connections)."})))
