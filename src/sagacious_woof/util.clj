(ns sagacious-woof.util
  "Distance function computations.")

(defn- grad->rad
  [number]
  (Math/toRadians number))

(defn rad-haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in kilometers
  by default.
  This function takes two vectors as argument, each one should be in the form
  [long lat]"
  [[long1 lat1] [long2 lat2]]
  (let [R  6372.8 ; kilometers
        h  (+ (Math/pow (Math/sin (/ (- lat2 lat1) 2)) 2)
              (* (Math/pow (Math/sin (/ (- long2 long1) 2)) 2)
                 (Math/cos lat2)
                 (Math/cos lat1)))]
    (* R 2 (Math/asin (Math/sqrt h)))))

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in degrees. The distance is computed in kilometers
  by default.
  This function takes two vectors as argument, each one should be in the form
  [long lat]"
  [p1 p2]
  (rad-haversine (mapv grad->rad p1) (mapv grad->rad p2)))

; (haversine [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106
; distance between paris and san francisco
; (* (haversine [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defn combinations
  [n coll]
  (if (= 1 n)
    (map list coll)
    (lazy-seq (when-let [[head & tail] (seq coll)]
                (concat (for [x (combinations (dec n) tail)] (cons head x))
                        (combinations n tail))))))
