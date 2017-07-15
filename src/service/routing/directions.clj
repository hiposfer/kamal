(ns service.routing.directions
  (:require [service.routing.graph.core :as route]
            [service.routing.osm :as osm]
            [service.routing.graph.algorithms :as alg]
            [service.routing.graph.protocols :as rp]
            [service.routing.utils.math :as math]))

(defn length
  "A very simple value computation function for Arcs in a graph.
  Returns a SimpleValue with the length of the arc"
  [arc _]
  (let [val (/ (:length arc) (get (:kind arc) osm/speeds osm/min-speed))]
    (route/->SimpleValue val)))

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

(defn- route
  "a route object as described in Mapbox directions docs.
  https://www.mapbox.com/api-documentation/#route-object"
  [graph trace]
  (let [linestring (geometry graph trace)]
    {:geometry    linestring
     :duration    (rp/time (val trace))
     :distance    (reduce + (map (fn [[lon lat] [lon2 lat2]] (math/haversine lon lat lon2 lat2))
                                 (:coordinates linestring)
                                 (rest (:coordinates linestring))))
     :weight      (rp/cost (val trace))
     :weight-name "routability" ;;TODO: give a proper name
     :legs        []})) ;; TODO

;; for the time being we only care about the coordinates of start and end
;; but looking into the future it is good to make like this such that we
;; can always extend it with more parameters
;; https://www.mapbox.com/api-documentation/#retrieve-directions
;; TODO: we still need to return the legs of a route
;; see https://github.com/mapbox/mapbox-gl-directions/issues/133
;; TODO: we need to handle special cases like
;; - src/dst not found
;; - shortest path not found (nil)
(defn direction
  "given a graph and a sequence of keywordized parameters according to
   https://www.mapbox.com/api-documentation/#retrieve-directions
   returns a response object similar to the one from Mapbox directions API

   Example:
   (direction graph :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]"
  [graph & parameters]
  (let [{:keys [coordinates]} parameters
        start     (brute-nearest graph (first coordinates))
        dst       (brute-nearest graph (last coordinates))
        traversal (alg/dijkstra graph :value-by length
                                      :start-from #{(key start)})
        trace     (reduce (fn [_ trace] (when (= (key trace) (key dst)) (reduced trace)))
                          nil
                          traversal)]
    (when-not (nil? trace)
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