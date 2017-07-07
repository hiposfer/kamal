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

(defn brute-nearest
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

(defn geometry
  "get a geojson linestring based on the route path"
  [graph trace]
  (let [coordinates (into [] (comp (map key)
                                   (map #(get graph %))
                                   (map #(vector (:lon %) (:lat %))))
                             (rp/path trace))]
    {:type "LineString" ;; trace path is in reverse order so we need to order it
     :coordinates (rseq coordinates)}))

(defn route
  "a route object as described in Mapbox directions docs.
  https://www.mapbox.com/api-documentation/#route-object"
  [graph trace]
  (let [linestring (geometry graph trace)]
    {:line        linestring
     :duration    (rp/time trace)
     :distance    (reduce + (map math/haversine
                                 (:coordinates linestring)
                                 (rest (:coordinates linestring))))
     :weight      (rp/cost trace)
     :weight-name "routability" ;;TODO: give a proper name
     :legs        []})) ;; TODO

;; for the time being we only care about the coordinates of start and end
;; but looking into the future it is good to make like this such that we
;; can always extend it with more parameters
;; https://www.mapbox.com/api-documentation/#retrieve-directions
(defn direction
  "given a graph and a sequence of keywordized parameters according to
   https://www.mapbox.com/api-documentation/#retrieve-directions
   returns a response object similar to the one from Mapbox directions API"
  [graph & parameters]
  (let [{:keys [coordinates]} parameters
        start     (brute-nearest graph (first coordinates))
        dest      (brute-nearest graph (last coordinates))
        traversal (alg/dijkstra graph :value-by length
                                      :direction ::alg/forward
                                      :start-from (key start))
        trace     (reduce #(when (= (key %) (key dest)) (reduced %)) traversal)]
    {:code "Ok"
     :waypoints (map (fn [point] {:name "wonderland"
                                  :location [(:lon point) (:lat point)]})
                     coordinates)
     :routes [(route graph trace)]}))

;(def graph (time (alg/biggest-component (time (osm/osm->graph "resources/osm/saarland.osm")))))
;(def performer (alg/dijkstra graph
;                             :value-by length
;                             :direction ::alg/forward
;                             :start-from #{(ffirst graph)}))
;
;(brute-nearest graph {:lat 49.354913, :lon 7.009614})
;(geometry graph (last performer))