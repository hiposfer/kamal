(ns service.routing.directions
  (:require [service.routing.graph.core :as route]
            [service.routing.osm :as osm]
            [service.routing.graph.algorithms :as alg]
            [service.routing.graph.protocols :as rp]))

(defn length
  "A very simple value computation function for Arcs in a graph.
  Returns a SimpleValue with the length of the arc"
  [arc _]
  (let [val (/ (:length arc) (get (:kind arc) osm/speeds osm/min-speed))]
    (route/->SimpleValue val)))

(defn euclidean-pow2
  "computes the squared euclidean distance between p1 and p2 being both of them
  {lat, lon} points. Use only if you interested in performance and not on the
   real value since the square root is an expensive computation"
  [p1 p2]
  (+ (Math/pow (- (:lat p1) (:lat p2)) 2)
     (Math/pow (- (:lon p1) (:lon p2)) 2)))

(defn euclidean
  "computes the euclidean distance between p1 and p2 being both of them
  geo-points"
  [p1 p2]
  (Math/sqrt (euclidean-pow2 p1 p2)))

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
   (brute-nearest graph point euclidean-pow2)))

(defn path
  "uses the traces structure to reconstruct the path taken to reach the
  destination id"
  [graph traces]
  (let [ids   (reverse traces)]
    (map #(get graph %) ids)))

(defn geometry
  "get a geojson linestring based on the route path"
  [graph trace]
  (let [coordinates (into [] (comp (map key)
                                   (map #(get graph %))
                                   (map #(vector (:lon %) (:lat %))))
                             (rp/path trace))]
    {:type "LineString"
     :coordinates coordinates}))

(defn route
  "a route object as described in Mapbox directions docs.
  https://www.mapbox.com/api-documentation/#route-object"
  [graph trace]
  {:line        (geometry graph trace)
   :duration    (rp/time trace)
   ;;distance   (haversine)])) TODO
   :weight      (rp/cost trace)
   :weight-name "routability";;todo: give a proper name
   :legs        []})

;(def graph (time (alg/biggest-component (time (osm/osm->graph "resources/osm/saarland.osm")))))
;(def performer (alg/dijkstra graph
;                             :value-by length
;                             :direction ::alg/forward
;                             :start-from #{(ffirst graph)}))
;
;(brute-nearest graph {:lat 49.354913, :lon 7.009614})
;(geometry graph (last performer))
