(ns sagacious-woof.core
  ;(:refer-clojure :exclude [* - + /  == <= >= not= ]) ;max has a bug. Waiting for fix in core.matrix
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  latitude and longitude in degrees. THe distance is computed in kilometers
  by default."
  [{long1 :long lat1 :lat} {long2 :long lat2 :lat}]
  (let [R 6372.8 ; kilometers
        dlat (Math/toRadians (- lat2 lat1))
        dlon (Math/toRadians (- long2 long1))
        lat1 (Math/toRadians lat1)
        lat2 (Math/toRadians lat2)
        foo  (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2)))
                (* (Math/sin (/ dlon 2)) (Math/sin (/ dlon 2))
                   (Math/cos lat1) (Math/cos lat2)))]
    (* R 2 (Math/asin (Math/sqrt foo)))))

(haversine {:lat 36.12 :long -86.67} {:lat 33.94 :long -118.40})
;=> 2887.2599506071106
; distance between paris and san francisco
(* (haversine {:lat 48.87 :long 2.33} {:lat 37.8 :long -122.4}) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles


