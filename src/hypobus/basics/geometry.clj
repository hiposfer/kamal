(ns hypobus.basics.geometry
  (:require [hypobus.util :as tool]
            [frechet-dist.core :refer [partial-frechet-dist]]))

(def ^:const ^:private MAX-DISTRUST 1)
(def ^:const ^:private MIN-WEIGHT (/ 1 (* 100 100))); 100 meters radious as deviation
(def ^:const ^:private RADIOUS 6372800); radious of the Earth in meters

(def ^:const ^:private MIN-DIST 30); meters
(def ^:const ^:private MAX-DIST 100); meters
(def ^:const ^:private MAX-GAP  300); meters

(defn- rad-haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in meters
  by default."
  [lat-1 lon-1 lat-2 lon-2]
  (let [h  (+ (Math/pow (Math/sin (/ (- lat-2 lat-1) 2)) 2)
              (* (Math/pow (Math/sin (/ (- lon-2 lon-1) 2)) 2)
                 (Math/cos lat-2)
                 (Math/cos lat-1)))]
    (* RADIOUS 2 (Math/asin (Math/sqrt h)))))

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in DEGREES. The distance is computed in meters
  by default. This function takes two hash-maps as argument with keys :lat :lon"
  [p1 p2]
  (rad-haversine (Math/toRadians (:lat p1)) (Math/toRadians (:lon p1))
                 (Math/toRadians (:lat p2)) (Math/toRadians (:lon p2))))

(defn vhaversine
  "same as haversine but takes two vector points as input."
  [[lat lon] [lat2 lon2]]
  (rad-haversine (Math/toRadians lat) (Math/toRadians lon)
                 (Math/toRadians lat2) (Math/toRadians lon2)))


; (haversine [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106
; distance between paris and san francisco
; (* (haversine [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defprotocol GeoDistance
  (-dist [object-1 object-2]
         [object-1 object-2 dist-fn]))

(defrecord GeoPoint [lat lon weight distrust])

(extend-type GeoPoint GeoDistance
  (-dist ([point-1 point-2]
          (haversine point-1 point-2))
         ([point-1 point-2 f]
          (f point-1 point-2))))

(extend-type clojure.lang.Sequential GeoDistance
  (-dist ([coll coll2]
          (partial-frechet-dist coll coll2 haversine))
         ([coll coll2 f]
          (f coll coll2))))

(defn point
  "create a point record. Two options are allowed; either a hash-map with the
  required key-vals or a direct lat lon instantiation."
  ([{lat :lat lon :lon dt :distrust w :weight :or {w MIN-WEIGHT, dt MAX-DISTRUST}}]
   {:pre  [(and (number? lat) (number? lon))]}
   (->GeoPoint lat lon w dt))
  ([lat lon]
   {:pre  [(and (number? lat) (number? lon))]}
  (->GeoPoint lat lon MIN-WEIGHT MAX-DISTRUST))
  ([lat lon weight distrust]
   {:pre  [(and (number? lat) (number? lon))]}
  (->GeoPoint lat lon weight distrust)))


(defn distance
  "computes the distance between two objects using function f.
  f defaults to the great-circle distance for points and the partial frechet
  distance for polygonal lines "
  ([o1 o2]
   (-dist o1 o2))
  ([o1 o2 f]
   (-dist o1 o2 f)))

(defn split-at-gaps
  "splits a curve into several ones if the distance between two consecutive
  points is greater than max-gap. f is the function used to calculate the
  distance between two points in the curve. Returns a list of curves."
  ([f coll]
   (split-at-gaps MAX-GAP f coll))
  ([max-gap f coll]
   (loop [parts    (transient [])
          currpart (transient [])
          tail     coll]
     (cond
       (empty? (rest tail)) (persistent! (conj! parts (persistent! (conj! currpart (first tail)))))
       (> max-gap (f (first tail) (second tail))) (recur parts (conj! currpart (first tail)) (rest tail))
       :else (recur (conj! parts (persistent! (conj! currpart (first tail)))) (transient []) (rest tail))))))

(defn- interpolate
  "returns n interpolated points between p_i (inclusive) and p_j (exclusive)"
  [p_i p_j n]
  (let [deltap (map / (map - p_j p_i) (repeat n))] ; size of each interval
    (for [a (range n)]
      (map + p_i (map * deltap (repeat a))))))

(defn tidy
  "tidy up a curve such that the distance between two points is not smaller
  than min-dist and not greater than max-dist. f is the function used to
  calculate the distance between two points in the curve. min-dist and max-dist
  default to 30 and 100 respectively"
  ([f coll]
   (tidy MIN-DIST MAX-DIST f coll))
  ([min-dist max-dist f coll]
  (let [rawcoll (map #(vector (:lat %) (:lon %)) coll)
        pij-dist (map f coll (rest coll))
        judge    (fn [index dist]
                   (cond
                     (> dist max-dist) (map #(apply point %) (interpolate (nth rawcoll index)
                                                                (nth rawcoll (inc index))
                                                                (Math/ceil (/ dist max-dist))))
                     (< dist min-dist) nil ; prepare for filtering
                     :else (list (point (nth coll index))))) ; OK point between the limits
        sampler  (comp (map-indexed judge) (remove nil?) (mapcat identity))
        new-coll (into [] sampler pij-dist)]
    (conj new-coll (point (last coll))))))
