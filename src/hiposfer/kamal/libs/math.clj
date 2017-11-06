(ns hiposfer.kamal.libs.math
  (:require [hiposfer.kamal.graph.protocols :as rp]))

;; Note in these scripts, I generally use
;; - latitude, longitude in degrees
;; - φ for latitude  in radians
;; - λ for longitude in radians
;; having found that mixing degrees & radians is often the easiest route to
;; head-scratching bugs...

(def RADIOUS 6372800); radious of the Earth in meters

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in DEGREES. The distance is computed in meters"
  ([lon-1 lat-1 lon-2 lat-2]
   (let [φ1 (Math/toRadians lat-1)
         φ2 (Math/toRadians lat-2)
         Δφ (Math/toRadians (- lat-2 lat-1))
         Δλ (Math/toRadians (- lon-2 lon-1))
         a  (+ (Math/pow (Math/sin (/ Δφ 2)) 2)
               (* (Math/pow (Math/sin (/ Δλ 2)) 2)
                  (Math/cos φ2)
                  (Math/cos φ1)))]
     (* RADIOUS 2 (Math/asin (Math/sqrt a)))))
  ([p1 p2]
   (if (and (seq? p1) (seq? p2))
     (apply haversine (concat p1 p2))
     (haversine (rp/lon p1) (rp/lat p1)
                (rp/lon p2) (rp/lat p2)))))

;; (frepos/distance [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106 km
; distance between paris and san francisco
; (* (frepos/distance [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defn euclidean-pow2
  "computes the squared euclidean distance between p1 and p2 being both of them
  {lat, lon} points. Use only if you interested in performance and not on the
   real value since the square root is an expensive computation"
  [p1 p2]
  (+ (Math/pow (- (rp/lat p1) (rp/lat p2)) 2)
     (Math/pow (- (rp/lon p1) (rp/lon p2)) 2)))

(defn euclidean
  "computes the euclidean distance between p1 and p2 being both of them
  geo-points"
  [p1 p2]
  (Math/sqrt (euclidean-pow2 p1 p2)))


(defn arc-length
  "given a sequence of [lon lat] points like those of geojson LineString
  returns the total distance traveled along the line"
  [coordinates]
  (reduce + (map haversine coordinates (rest coordinates))))

; sources
; http://www.movable-type.co.uk/scripts/latlong.html
(defn bearing
  "return a Number between 0 and 360 indicating the clockwise angle from true
   north to the direction of travel (p1 -> p2)"
  [p1 p2]
  (let [φ1 (Math/toRadians (rp/lat p1))
        φ2 (Math/toRadians (rp/lat p2))
        Δλ (Math/toRadians (- (rp/lon p2) (rp/lon p1)))
        ; actual computation
        y (* (Math/sin Δλ) (Math/cos φ2))
        x (- (* (Math/cos φ1) (Math/sin φ2))
             (* (Math/sin φ1) (Math/cos φ2) (Math/cos Δλ)))]
    (rem (+ (Math/toDegrees (Math/atan2 y x)) 360)
         360)));

;; TODO: consider using geohashes
;; http://www.bigfastblog.com/geohash-intro
(defn lexicographic-coordinate
  "comparator function to order nodes in a graph
  WARNING: this assumes that two points cannot occupy the same
  space. e.g. no 3D points since two point with different height
  but equal lat, lon would collide"
  [x y]
  (compare [(rp/lat x) (rp/lon x)]
           [(rp/lat y) (rp/lon y)]))