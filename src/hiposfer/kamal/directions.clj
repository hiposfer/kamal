(ns hiposfer.kamal.directions
  (:require [hiposfer.kamal.osm :as osm]
            [hiposfer.kamal.graph.algorithms :as alg]
            [hiposfer.kamal.graph.protocols :as rp]
            [hiposfer.kamal.libs.math :as math]
            [clojure.data.avl :as avl]))
            ;[clojure.set :as set]
            ;[cheshire.core :as cheshire]))
            ;[cheshire.core :as cheshire]))

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

(def ->coordinates (juxt rp/lon rp/lat))

;; WARNING: we assume that we only traverse outgoing arcs
;; and that there is only one arc connecting src & dst
(defn- link
  "find the link that connects src and dst and returns it"
  [graph src-trace dst-trace]
  (some #(when (= (key dst-trace) (rp/dst %)) %)
         (rp/successors (graph (key src-trace)))))


(defn duration
  "A very simple value computation function for Arcs in a graph.
  Returns the time it takes to go from arc src to dst based on osm/speeds"
  [graph arc _] ;; 1 is a simple value used for test whenever no other value would be suitable
  (let [src    (graph (rp/src arc))
        dst    (graph (rp/dst arc))
        length (math/haversine (rp/lon src) (rp/lat src)
                               (rp/lon dst) (rp/lat dst))]
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
  (let [location     (->coordinates (graph (key (first (first piece)))))
        pre-bearing  (math/bearing (graph (key (first (first prev-piece))))
                                   (graph (key (first (first piece)))))
        post-bearing (math/bearing (graph (key (first (first piece))))
                                   (graph (key (first (first next-piece)))))
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
  (let [linestring (linestring network (map (comp key first) (concat piece [(first next-piece)])))]
    {:distance (math/arc-length (:coordinates linestring))
     :duration (- (val (first (first next-piece)))
                  (val (first (first piece))))
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
  (let [way-ids     (sequence (comp (map (partial link graph))
                                    (map rp/way))
                              trail (rest trail))
        ways&traces (map vector trail (concat way-ids [(last way-ids)]))
        pieces      (partition-by #(:name (ways (second %))) ways&traces)]
    {:distance   (math/arc-length (:coordinates (linestring network (map key trail))))
     :duration   (rp/cost (val (last trail)))
     :steps      (route-steps network steps pieces)
     :summary    "" ;; TODO
     :annotation []})) ;; TODO

;https://www.mapbox.com/api-documentation/#route-object
(defn- route
  "a route through (potentially multiple) waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps rtrail]
  ;; a single trace is returned for src = dst
  (let [trail (if (= 1 (count rtrail))
                 (repeat 2 (first rtrail))
                 (rseq (into [] rtrail)))
        leg    (route-leg network steps trail)]
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
   returns a response object similar to the one from Mapbox directions API

   Example:
   (direction network :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]"
  [{:keys [graph ways neighbours] :as network} & params]
  (let [{:keys [coordinates steps radiuses alternatives language]} params
        start     (avl/nearest neighbours <= (first coordinates))
        dst       (avl/nearest neighbours <= (last coordinates))]
    (if (or (nil? start) (nil? dst)) {:code "NoSegment"}
      (let [traversal (alg/dijkstra (:graph network)
                                    #(duration graph %1 %2)
                                    #{(val start)})
            rtrail     (alg/shortest-path (val dst) traversal)]
        (if (nil? rtrail) {:code "NoRoute"}
          {:code "Ok"
           :routes [(route network steps rtrail)]
           :waypoints [{:name (str (:name (ways (some rp/way (rp/successors (key start))))))
                        :location (->coordinates (key start))}
                       {:name (str (:name (ways (some rp/way (rp/successors (key dst))))))
                        :location (->coordinates (key dst))}]})))))


;(def conn (alg/biggest-component (:graph @(:network (:grid hiposfer.kamal.dev/system)))))
;(def origin      (->coordinates (val (rand-nth (seq conn)))))
;(def destination (->coordinates (val (rand-nth (seq conn)))))
;
;origin
;(math/haversine origin
;  (key (avl/nearest (into (avl/sorted-map-by math/lexicographic-coordinate)
;                          (set/map-invert conn))
;                    >=
;                    origin)))
;;
;(direction {:graph      conn
;            :ways       (:graph @(:network (:grid hiposfer.kamal.dev/system)))
;            :neighbours (into (avl/sorted-map-by math/lexicographic-coordinate)
;                              (set/map-invert conn))}
;           :coordinates [origin destination]
;           :steps true)

;(def result (direction {:graph      conn
;                        :ways       (:graph @(:network (:grid hiposfer.kamal.dev/system)))
;                        :neighbours (into (avl/sorted-map-by math/lexicographic-coordinate)
;                                          (set/map-invert conn))}
;                       :coordinates [origin destination]
;                       :steps true))
;
;(time (count (alg/biggest-component (:graph @(:network (:grid hiposfer.kamal.dev/system))))))
;
;(time (count (:graph @(:network (:grid hiposfer.kamal.dev/system)))))
;
;(take 10 (:ways @(:network (:grid hiposfer.kamal.dev/system))))
;
;result
;(spit "resources/linestring.json"
;  (cheshire/generate-string (:geometry (first (:routes result)))))

;(keys (:grid dev/system))