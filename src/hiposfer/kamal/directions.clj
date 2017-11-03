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

(defn- wayver
  "get the way id based on the src/dst id of the traversed nodes"
  [graph src dst]
  (rp/way (reduce (fn [_ arc] (when (= dst (rp/dst arc)) (reduced arc)))
                  nil
                  (rp/successors (graph src)))))


(defn duration
  "A very simple value computation function for Arcs in a graph.
  Returns the time it takes to go from arc src to dst based on osm/speeds"
  [graph arc _] ;; 1 is a simple value used for test whenever no other value would be suitable
  (let [src    (graph (rp/src arc))
        dst    (graph (rp/dst arc))
        length (math/haversine (rp/lon src) (rp/lat src)
                               (rp/lon dst) (rp/lat dst))]
    (/ length osm/walking-speed)))

(defn- geometry
  "get a geojson linestring based on the route path"
  ([network trace]
   (geometry network trace nil))
  ([{:keys [graph]} trace last-id]
    ;; include the last point in the geometry
   (let [coordinates (transduce (comp (halt-when #(= (key %) last-id)
                                        (fn [r h] (conj r (->coordinates (graph (key h))))))
                                      (map key)
                                      (map graph)
                                      (map ->coordinates))
                                conj
                                []
                                trace)]
     {:type "LineString" ;; trace path is in reverse order so we need to order it
      :coordinates (rseq coordinates)})))

;; WARNING: we assume that we only traverse outgoing arcs
;; and that there is only one arc connecting src & dst
(defn- partition-by-street-name
  "split the trace into a sequence of traces using the street name as separator"
  [{:keys [graph ways]} trace]
  (let [way-ids (map (fn [dst src] (wayver graph (key src) (key dst)))
                     trace
                     (rest trace))
        ;; last: little hack to get the way of the first point
        traces  (sequence (comp (map vector)
                                (partition-by (fn [[_ id]] (:name (ways id))))
                                (map #(map first %))) ;; get the path traces
                          trace
                          (concat way-ids [(last way-ids)]))
        streets (sequence (comp (partition-by #(:name (ways %)))
                                (map first)) ;; the first way id suffices
                          way-ids)]
        ;_ (println traces)]
    (map vector streets traces)))


;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [{:keys [graph ways]}
   [way-id-to trace-to :as destination]
   [_ trace  :as origin]]
  (let [location     (->coordinates (graph (key (first trace))))
        pre-bearing  (math/bearing (->coordinates (graph (key (second trace))))
                                   (->coordinates (graph (key (first trace)))))
        post-bearing (math/bearing (->coordinates (graph (key (first trace))))
                                   (->coordinates (graph (key (first trace-to)))))
        angle        (mod (+ 360 (- post-bearing pre-bearing)) 360)
        modifier     (val (last (subseq bearing-turns <= angle)))
        way-name     (:name (ways way-id-to))
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
(defn- step
  "includes one StepManeuver object and travel to the following RouteStep"
  [{:keys [ways] :as network}
   [way-id-to trace-to :as destination]
   [_ trace :as origin]]
  (let [linestring (geometry network trace-to (key (first trace)))]
    {:distance (math/arc-length (:coordinates linestring))
     :duration (- (rp/cost (val (first trace-to)))
                  (rp/cost (val (first trace))))
     :geometry linestring
     :name     (str (:name (ways way-id-to)))
     :mode     "walking" ;;TODO this should not be hardcoded
     :maneuver (maneuver network destination origin)
     :intersections []})) ;; TODO


;https://www.mapbox.com/api-documentation/#routeleg-object
(defn- route-leg
  "a route between only two waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps trace]
  (let [edge-case   (= 1 (count trace)) ;; happens when origin = destination
        linestring  (geometry network trace)
        ;; prevent computation when edge case
        ways&traces (lazy-seq (partition-by-street-name network trace))]
    {:distance     (math/arc-length (:coordinates linestring))
     :duration     (rp/cost (val (first trace)))
     ;; TODO: add depart and arrival steps
     :steps        (if (or (not steps) edge-case) []
                     (reverse (map step (repeat network)
                                        ways&traces
                                        (rest ways&traces))))
     :summary      "" ;; TODO
     :annotation   [] ;; TODO
     :geometry     (if-not edge-case linestring
                     (assoc linestring
                            :coordinates
                            (repeat 2 (first (:coordinates linestring)))))}))

;https://www.mapbox.com/api-documentation/#route-object
(defn- route
  "a route through (potentially multiple) waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps trace]
  (let [leg       (route-leg network steps trace)]
    {:geometry    (:geometry leg)
     :duration    (:duration leg)
     :distance    (:distance leg)
     :weight      (:duration leg)
     :weight_name "time"
     :legs        [(dissoc leg :geometry)]}))

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
            trace     (alg/shortest-path (val dst) traversal)]
        (if (nil? trace) {:code "NoRoute"}
          {:code "Ok"
           :routes [(route network steps trace)]
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