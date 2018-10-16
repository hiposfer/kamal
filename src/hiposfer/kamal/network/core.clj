(ns hiposfer.kamal.network.core
  (:require [hiposfer.kamal.network.algorithms.protocols :as np])
  (:import (clojure.lang APersistentMap IPersistentVector)
           (ch.hsr.geohash GeoHash)))

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

;; A vector of two numbers can be interpreted as a Point
;; according to the GeoJson standard
(extend-type IPersistentVector
  np/GeoCoordinate
  (lat [this] (second this))
  (lon [this] (first this)))

(extend-type APersistentMap
  np/GeoCoordinate
  (lat [this] (:lat this))
  (lon [this] (:lon this)))

;; A Number is the simplest way to represent the cost of traversing an Arc
;; in a Graph. Useful for Dijkstra and similar algorithms
(extend-type Number
  np/Valuable
  (cost [this] this))
