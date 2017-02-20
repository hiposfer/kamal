(ns hypobus.basics.geometry
  (:require [frechet-dist.core :as frechet]
            [frechet-dist.protocols :as frepos]
            [hypobus.utils.tool :as tool])
  (:import (clojure.lang IPersistentMap Sequential)))

;; TODO: most of these declarations should be dynamic at some point
(def MAX-DISTRUST 1.0)
(def MIN-WEIGHT (/ 1 (* 100 100))); 100 meters radious as deviation
(def RADIOUS 6372800); radious of the Earth in meters

(def MIN-DIST 30); meters
(def MAX-DIST 100); meters
(def MAX-GAP  300); meters

(defn euclidean-pow2
  "computes the squared euclidean distance between p1 and p2 being both of them
  geo-points. Use only if you interested in performance and not on the real value
  since the square root is an expensive computation"
  [p1 p2]
  (+ (Math/pow (- (:lat p1) (:lat p2)) 2)
     (Math/pow (- (:lon p1) (:lon p2)) 2)))

(defn euclidean
  "computes the euclidean distance between p1 and p2 being both of them
  geo-points"
  [p1 p2]
  (Math/sqrt (euclidean-pow2 p1 p2)))

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in meters
  by default."
  [lon-1 lat-1 lon-2 lat-2]
  (let [h  (+ (Math/pow (Math/sin (/ (- lat-2 lat-1) 2)) 2)
              (* (Math/pow (Math/sin (/ (- lon-2 lon-1) 2)) 2)
                 (Math/cos lat-2)
                 (Math/cos lat-1)))]
    (* RADIOUS 2 (Math/asin (Math/sqrt h)))))

;; (frepos/distance [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106 km
; distance between paris and san francisco
; (* (frepos/distance [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defrecord HypoPoint [^double lon
                      ^double lat
                      ^double weight
                      ^double distrust])

(extend-protocol frepos/Distance
  HypoPoint ;; point as HypoPoint record
  (frepos/distance [p1 p2]
    (haversine (Math/toRadians (:lon p1)) (Math/toRadians (:lat p1))
               (Math/toRadians (:lon p2)) (Math/toRadians (:lat p2))))
  IPersistentMap ;; point as hash-map
  (frepos/distance [p1 p2]
    (haversine (Math/toRadians (:lon p1)) (Math/toRadians (:lat p1))
               (Math/toRadians (:lon p2)) (Math/toRadians (:lat p2))))
  Sequential ;; point as a lat,lon tuple
  (frepos/distance [[lon lat] [lon2 lat2]]
    (haversine (Math/toRadians lon)  (Math/toRadians lat)
               (Math/toRadians lon2) (Math/toRadians lat2))))

(defn gaps
  "returns a reducing function to use with partition-by such that a curve can
  be partitioned into several onees if the distance between two consecutive
  points is greater than max-gap. f is the function used to calculate the
  distance between two points in the curve. f defaults to frechet.protocols/distance
  and max-gap defaults to hypobus.basics.geometry/MAX-GAP"
  ([]
   (gaps MAX-GAP frepos/distance))
  ([max-gap]
   (gaps max-gap frepos/distance))
  ([max-gap f]
   (let [prev (volatile! ::ok)]
     (fn [value]
       (let [prior @prev]
         (vreset! prev value)
         (if (or (identical? prior ::ok) (> max-gap (f prior value)))
           ::ok
           value))))))
;; example
;; (partition-by (gaps 2 #(Math/abs (double (- %1 %2)))) [2 3 4 7 6 1])
;; => ((2 3 4)(7 6)(1))

(defn- interpolate
  "returns n interpolated points between mi (inclusive) and mj (exclusive)
  where mi and mj are both hash-maps with the same shape"
  [mi mj n]
  (let [delta (map / (map - (vals mj) (vals mi))
                     (repeat n))  ; size of each interval
        pdelta (zipmap (keys mi) delta)]
    (for [i (range n)]
      (merge-with + mi (tool/update-vals pdelta #(* % i))))))
;; example
;; (interpolate {:a 0 :b 0} {:a 5 :b 5} 3)

(defn tidy
  "tidy up a curve such that the distance between two points is not smaller
  than min-dist and not greater than max-dist. f is the function used to
  calculate the distance between two points in the curve. min-dist and max-dist
  default to hypobus.basics.geometry/MIN-DIST and hypobus.basics.geometry/MAX-DIST
  respectively"
  ([hypocurve]
   (tidy hypocurve MIN-DIST MAX-DIST frepos/distance))
  ([hypocurve min-dist max-dist]
   (tidy hypocurve MIN-DIST MAX-DIST frepos/distance))
  ([hypocurve min-dist max-dist f]
   (let [pij-dist (map f hypocurve (rest hypocurve))
         judge    (fn [index dist]
                    (cond
                      (> dist max-dist) (interpolate (nth hypocurve index)
                                                     (nth hypocurve (inc index))
                                                     (Math/ceil (/ dist max-dist)))
                      (< dist min-dist) nil ; prepare for filtering
                      :else (list (nth hypocurve index)))) ; OK point between the limits
         sampler  (comp (map-indexed judge) (remove nil?) (mapcat identity))
         new-coll (into [] sampler pij-dist)]
     (conj new-coll (last hypocurve)))))
