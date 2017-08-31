(ns service.routing.directions
  (:require [service.routing.graph.core :as route]
            [service.routing.osm :as osm]
            [service.routing.graph.algorithms :as alg]
            [service.routing.graph.protocols :as rp]
            [service.routing.utils.math :as math]
            [service.routing.spec :as specs]))


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
  "search the nearest node in graph to point using the distance function f.
  f defaults to the euclidean distance squared"
  ([graph point f]
   (reduce (fn [best entry] (if (< (f point (second entry))
                                   (f point (second best)))
                              entry
                              best))
           (first graph)
           graph))
  ([graph point]
   (brute-nearest graph point math/euclidean-pow2)))

(defn- geometry
  "get a geojson linestring based on the route path"
  [graph trace]
  (let [coordinates (into [] (comp (map key)
                                   (map #(get graph %))
                                   (map #(vector (:lon %) (:lat %))))
                             (rp/path trace))]
    {:type "LineString" ;; trace path is in reverse order so we need to order it
     :coordinates (rseq coordinates)}))

;https://www.mapbox.com/api-documentation/#routestep-object
(defn- step
  "includes one StepManeuver object and travel to the following RouteStep"
  [graph trace])


;https://www.mapbox.com/api-documentation/#routeleg-object
(defn- route-leg
  "a route between only two waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [graph trace]
  (let [linestring (geometry graph trace)]
    {:distance     (math/arc-length (:coordinates linestring))
     :duration     (rp/cost (val trace))
     :steps        []
     :summary      "" ;; TODO
     :annotation   [] ;; TODO
     :geometry     linestring}))

;https://www.mapbox.com/api-documentation/#route-object
(defn- route
  "a route through (potentially multiple) waypoints

  WARNING: we dont support multiple waypoints yet !!"
  [graph & traces]
  (let [leg       (route-leg graph traces)]
    {:geometry    (:geometry leg)
     :duration    (:duration route-leg)
     :distance    (:distance route-leg)
     :weight      (:duration route-leg)
     :weight-name "time"
     :legs        [(dissoc leg :geometry)]}))

;; for the time being we only care about the coordinates of start and end
;; but looking into the future it is good to make like this such that we
;; can always extend it with more parameters
;; https://www.mapbox.com/api-documentation/#retrieve-directions
(defn direction
  "given a graph and a sequence of keywordized parameters according to
   https://www.mapbox.com/api-documentation/#retrieve-directions
   returns a response object similar to the one from Mapbox directions API

   Example:
   (direction graph :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]"
  [graph & params]
  (let [{:keys [coordinates steps radiuses alternatives language]} params
        start     (brute-nearest graph (first coordinates))
        dst       (brute-nearest graph (last coordinates))
        traversal (alg/dijkstra graph :value-by #(duration graph %1 %2)
                                :start-from #{(key start)})
        trace     (reduce (fn [_ trace] (when (= (key trace) (key dst)) (reduced trace)))
                          nil
                          traversal)]
    (if (nil? trace)
      {:code "NoRoute"}
      {:code "Ok"
       :waypoints (map (fn [point] {:name "wonderland"
                                    :location [(:lon point) (:lat point)]})
                       coordinates)
       :routes [(route graph trace)]})))


;(println (direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))

;(def graph (time (alg/biggest-component (time (osm/osm->graph "resources/osm/saarland.osm")))))
;(def performer (alg/dijkstra graph
;                             :value-by length
;                             :direction ::alg/forward
;                             :start-from #{(ffirst graph)}))
;
;(brute-nearest graph {:lat 49.354913, :lon 7.009614})
;(geometry graph (last performer))