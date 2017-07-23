(ns service.routing.utils.math)


(def RADIOUS 6372800); radious of the Earth in meters

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in meters
  by default."
  ([lon-1 lat-1 lon-2 lat-2]
   (let [h  (+ (Math/pow (Math/sin (/ (- lat-2 lat-1) 2)) 2)
               (* (Math/pow (Math/sin (/ (- lon-2 lon-1) 2)) 2)
                  (Math/cos lat-2)
                  (Math/cos lat-1)))]
     (* RADIOUS 2 (Math/asin (Math/sqrt h)))))
  ([p1 p2]
   (if (and (sequential? p1) (sequential? p2))
     (let [[lon  lat]   p1
           [lon2 lat2] p2]
       (haversine lon lat lon2 lat2))
     (haversine (:lon p1) (:lat p1) (:lon p2) (:lat p2)))))

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

;; (frepos/distance [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106 km
; distance between paris and san francisco
; (* (frepos/distance [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

(defn arc-length
  "given a sequence of [lon lat] points like those of geojson LineString
  returns the total distance traveled along the line"
  [coordinates]
  (reduce + (map (fn [[lon lat] [lon2 lat2]] (haversine lon lat lon2 lat2))
                 coordinates
                 (rest coordinates))))
