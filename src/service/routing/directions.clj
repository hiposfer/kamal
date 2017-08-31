(ns service.routing.directions
  (:require [service.routing.osm :as osm]
            [service.routing.graph.algorithms :as alg]
            [service.routing.graph.protocols :as rp]
            [service.routing.utils.math :as math]
            [clojure.string :as str]
            [cheshire.core :as cheshire]))

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

(defn duration
  "A very simple value computation function for Arcs in a graph.
  Returns the time it takes to go from arc src to dst based on osm/speeds"
  [graph arc _] ;; 1 is a simple value used for test whenever no other value would be suitable
  (let [src    (get graph (rp/src arc))
        dst    (get graph (rp/dst arc))
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
   (let [locator     (juxt rp/lon rp/lat)
         coordinates (transduce (comp (halt-when #(= (key %) last-id)
                                        (fn [r h] (conj r (locator (get graph (key h))))))
                                      (map key)
                                      (map #(get graph %))
                                      (map locator))
                                conj
                                []
                                (rp/path trace))]
     {:type "LineString" ;; trace path is in reverse order so we need to order it
      :coordinates (rseq coordinates)})))

(defn- split-ways
  "split the trace into a sequence of traces using the way id as separator"
  [graph trace] ;; WARNING: we assume that we only traverse outgoing arcs
  (let [ways    (sequence (comp (map #(vector (key %1) (key %2)))
                                (map (fn [[src dst]]
                                       (rp/way (get (into {} (rp/successors (get graph src)))
                                                    dst)))))
                         (rp/path trace)
                         (rest (rp/path trace)))
        traces  (sequence (comp (map vector)
                                (partition-by second)
                                (map #(map first %)))
                          (rp/path trace)
                          (concat ways [(last ways)]))]
    ;; last: little hack to get the way of the first point
    (into [] (map vector (dedupe ways) traces))))


;; https://www.mapbox.com/api-documentation/#stepmaneuver-object
(defn- maneuver
  "returns a step manuever"
  [{:keys [graph ways]} traces way-id]
  ;(println "path" (rp/path (first traces)))
  (let [traces       (if (> (count traces) 1) traces
                       (concat traces [(first traces)]))
        coordinate   (juxt rp/lon rp/lat)
        location     (coordinate (get graph (key (first traces))))
        pre-bearing  (math/bearing (coordinate (get graph (key (last traces))))
                                   (coordinate (get graph (key (last (butlast traces))))))
        post-bearing (math/bearing (coordinate (get graph (key (first traces))))
                                   (coordinate (get graph (key (first (rest traces))))))
        angle    (mod (+ 360 (- post-bearing pre-bearing)) 360)
        modifier (val (last (subseq bearing-turns <= angle)))
        way-name (:name (get ways way-id))
        instruction (str/capitalize (str "take " modifier (when way-name (str " on " way-name))))]
    {:location location
     ;; todo: implement maneuver type
     ;https://www.mapbox.com/api-documentation/#maneuver-types
     :type     "turn"
     :bearing_before pre-bearing
     :bearing_after  post-bearing
     :modifier modifier
     :instruction instruction}))

;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step
  "includes one StepManeuver object and travel to the following RouteStep"
  [{:keys [ways] :as network} [way-id traces]]
  (let [linestring (geometry network (first traces) (key (last traces)))]
    {:distance (math/arc-length (:coordinates linestring))
     :duration (- (rp/cost (val (first traces)))
                  (rp/cost (val (last traces))))
     :geometry (:coordinates linestring)
     :name     (str (:name (get ways way-id)))
     :mode     "walking" ;;todo this should not be hardcoded
     :maneuver (maneuver network traces way-id)
     :intersections nil})) ;; todo


;https://www.mapbox.com/api-documentation/#routeleg-object
(defn- route-leg
  "a route between only two waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [network steps trace]
  (let [linestring (geometry network trace)]
    {:distance     (math/arc-length (:coordinates linestring))
     :duration     (rp/cost (val trace))
     :steps        (if steps (map step (repeat network)
                                       (split-ways (:graph network) trace))
                     [])
     :summary      "" ;; TODO
     :annotation   [] ;; TODO
     :geometry     linestring}))

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
     :weight-name "time"
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
  [network & params]
  (let [{:keys [coordinates steps radiuses alternatives language]} params
        start     (brute-nearest network (first coordinates))
        dst       (brute-nearest network (last coordinates))
        traversal (alg/dijkstra (:graph network)
                                :value-by #(duration (:graph network) %1 %2)
                                :start-from #{(key start)})
        trace     (reduce (fn [_ trace] (when (= (key trace) (key dst)) (reduced trace)))
                          nil
                          traversal)]
    (if (nil? trace)
      {:code "NoRoute"}
      {:code "Ok"
       :waypoints (map (fn [point] {:name "wonderland" ;;todo
                                    :location [(rp/lon point) (rp/lat point)]})
                       coordinates)
       :routes [(route network steps trace)]})))

;(defonce network (time (update (time (osm/osm->network "resources/osm/saarland.osm"))
;                               :graph
;                               alg/biggest-component)))
;
;(def result (direction network
;              :coordinates [((juxt rp/lon rp/lat) (val (rand-nth (seq (:graph network)))))
;                            ((juxt rp/lon rp/lat) (val (rand-nth (seq (:graph network)))))]
;              :steps true))
;
;result
;
;(spit "resources/linestring.json"
;  (cheshire/generate-string (:geometry (first (:routes result)))))