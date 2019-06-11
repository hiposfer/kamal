(ns hiposfer.kamal.router.algorithms.core
  (:require [hiposfer.kamal.router.algorithms.protocols :as np])
  (:import (ch.hsr.geohash GeoHash)))

;; a Point is a simple longitude, latitude pair used to
;; represent the geometry of a way in Open Street Maps
(defrecord Location [^double lon ^double lat]
  np/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon)
  ;; http://www.bigfastblog.com/geohash-intro
  ;; HACK: we use a GeoHash to avoid ordering by lat and lon.
  ;; We then use Location inside Datascript instead of lat, lon
  ;; separatedly which gives us nearest neighbour search for free :)
  Comparable
  (compareTo [_ y]
    (compare (GeoHash/withBitPrecision lat lon  64)
             (GeoHash/withBitPrecision (np/lat y) (np/lon y) 64))))

;; A Number is the simplest way to represent the cost of traversing an Arc
;; in a Graph. Useful for Dijkstra and similar algorithms
(extend-type Number
  np/Valuable
  (cost [this] this))

#_(defn- components
    "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of a undirected graph

   NOTE: only relevant for pedestrian routing"
    [network router settled]
    (if (= (count (nodes network)) (count settled)) (list)
      (let [start     (some #(and (not (settled %)) %)
                             (nodes network))
            connected (sequence (comp (map first) (map key))
                                (dijkstra router #{start}))]
       (cons connected (lazy-seq (components network router (into settled connected)))))))

;; note for specs: the looner of the looner should be empty
#_(defn looners
    "returns a sequence of ids that can be removed from the graph
  because they are not part of the strongest connected component

  NOTE: only relevant for pedestrian routing"
    [network router]
    (let [subsets   (components network router #{})
          connected (into #{} (apply max-key count subsets))]
      (remove #(contains? connected %) (nodes network))))
