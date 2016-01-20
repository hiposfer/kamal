(ns sagacious-woof.util
  "collection of utility functions")

(defn- grad->rad
  [number]
  (Math/toRadians number))

(defn- rad-haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in kilometers
  by default.
  This function takes two hash-maps as argument with keys :lat :long"
  [p1 p2]
  (let [R  6372.8 ; kilometers
        h  (+ (Math/pow (Math/sin (/ (- (:lat p2) (:lat p1)) 2)) 2)
              (* (Math/pow (Math/sin (/ (- (:long p2) (:long p1)) 2)) 2)
                 (Math/cos (:lat p2))
                 (Math/cos (:lat p1))))]
    (* R 2 (Math/asin (Math/sqrt h)))))

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in DEGREES. The distance is computed in kilometers
  by default.
  This function takes two vectors as argument, each one should be in the form
  [long lat]"
  [p1 p2]
  (rad-haversine {:lat (Math/toRadians (:lat p1)) :long (Math/toRadians (:long p1))}
                 {:lat (Math/toRadians (:lat p2)) :long (Math/toRadians (:long p2))}))

; (haversine [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106
; distance between paris and san francisco
; (* (haversine [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defn combinations
  "returns a lazy sequence of all the possible combinations of the elements in
  coll in groups of n members"
  [n coll]
  (if (= 1 n)
    (map list coll)
    (lazy-seq (when-let [[head & tail] (seq coll)]
                (concat (for [x (combinations (dec n) tail)] (cons head x))
                        (combinations n tail))))))
;(combinations 2 [:a :b :c])
;((:a :b) (:a :c) (:b :c))
(defn all-pairs [coll] (combinations 2 coll))

(defn vec->map
  [point]
  {:long (first point) :lat  (second point)})

(defn coords->geo
  [coll]
  (map vec->map coll))

(def last-index (comp dec count))
