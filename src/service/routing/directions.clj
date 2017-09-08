(ns service.routing.directions
  (:require [service.routing.osm :as osm]
            [service.routing.graph.algorithms :as alg]
            [service.routing.graph.protocols :as rp]
            [service.routing.libs.math :as math]
            [service.routing.graph.generators :as g]
            [clojure.test.check.generators :as gen]))
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

(defn- brute-nearest
  "search the nearest node in network to point using the distance function f.
  f defaults to the euclidean distance squared"
  ([network point f]
   (reduce (fn [best entry] (if (< (f point (second entry))
                                   (f point (second best)))
                              entry
                              best))
           (first (:graph network))
           (:graph network)))
  ([network point]
   (brute-nearest network point math/euclidean-pow2)))

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
                                (rp/path trace))]
     {:type "LineString" ;; trace path is in reverse order so we need to order it
      :coordinates (rseq coordinates)})))

;; WARNING: we assume that we only traverse outgoing arcs
;; and that there is only one arc connecting src & dst
(defn- partition-by-street-name
  "split the trace into a sequence of traces using the way id as separator"
  [{:keys [graph ways]} trace]
  (let [way-ids (sequence (map (fn [dst src] (wayver graph (key src) (key dst))))
                          (rp/path trace)
                          (rest (rp/path trace)))
        ;; last: little hack to get the way of the first point
        traces  (sequence (comp (map vector)
                                (partition-by (fn [[_ wid]] (:name (ways wid))))
                                (map #(map first %)) ;; get the path traces
                                (map first)) ;; get only the first trace
                          (rp/path trace)
                          (concat way-ids [(last way-ids)]))
        streets (sequence (comp (partition-by #(:name (ways %)))
                                (map first)) ;; the first way id suffices
                          way-ids)]
        ;_ (println traces)]
    (sequence (map vector) streets traces)))


;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [{:keys [graph ways]} [way-id-to trace-to :as destination] [_ trace  :as origin]]
  (let [location     (->coordinates (graph (key trace)))
        pre-bearing  (math/bearing (->coordinates (graph (key (second (rp/path trace)))))
                                   (->coordinates (graph (key trace))))
        post-bearing (math/bearing (->coordinates (graph (key trace)))
                                   (->coordinates (graph (key trace-to))))
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
  [{:keys [ways] :as network} [way-id-to trace-to :as destination] [_ trace :as origin]]
  (let [linestring (geometry network trace-to (key trace))]
    {:distance (math/arc-length (:coordinates linestring))
     :duration (- (rp/cost (val trace-to))
                  (rp/cost (val trace)))
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
  (let [edge-case   (= 1 (count (rp/path trace))) ;; happens when origin = destination
        linestring  (geometry network trace)
        ;; prevent computation when edge case
        ways&traces (lazy-seq (partition-by-street-name network trace))]
    {:distance     (math/arc-length (:coordinates linestring))
     :duration     (rp/cost (val trace))
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
  [{:keys [graph ways] :as network} & params]
  (let [{:keys [coordinates steps radiuses alternatives language]} params
        start     (brute-nearest network (first coordinates))
        dst       (brute-nearest network (last coordinates))
        traversal (alg/dijkstra (:graph network)
                                :value-by #(duration graph %1 %2)
                                :start-from #{(key start)})
        trace     (reduce (fn [_ trace] (when (= (key trace) (key dst)) (reduced trace)))
                          nil
                          traversal)]
    (if (nil? trace)
      {:code "NoRoute"}
      {:code "Ok"
       :routes [(route network steps trace)]
       :waypoints [{:name (or "" (:name (ways (some rp/way (rp/successors (graph (key start)))))))
                    :location (->coordinates (val start))}
                   {:name (or "" (:name (ways (some rp/way (rp/successors (graph (key start)))))))
                    :location (->coordinates (val dst))}]})))

;(defonce network (time (update (time (osm/osm->network "resources/osm/saarland.osm"))
;                               :graph
;                               alg/biggest-component)))
;
;network

;(def origin [7.0016269 49.2373449])
;(def destination [7.0240812 49.6021303])

;(def origin      ((juxt rp/lon rp/lat) (val (rand-nth (seq (:graph network))))))
;(def destination ((juxt rp/lon rp/lat) (val (rand-nth (seq (:graph network))))))
;
;(def result (direction network
;                       :coordinates [origin destination]
;                       :steps true))
;
;result
;(:steps (first (:legs (first (:routes result)))))
;
;(spit "resources/linestring.json"
;  (cheshire/generate-string (:geometry (first (:routes result)))))