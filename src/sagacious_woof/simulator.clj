(ns sagacious-woof.simulator
  "functions used to simulate partial curves"
  (:require [clojure.test.check.generators :as gen]))

(defn- rand-intervals
  "create num-curves random intervals that span between 0 (included) and
  length (included). The minimum distance between the start and the end is
  set to span"
  [length num-curves span]
  (let [start      (gen/choose 0 length)
        end        (gen/choose 0 length)
        interval   (gen/such-that (fn [[a b]] (>= (- b a) span))
                                  (gen/tuple start end)
                                  100)]
    (gen/sample interval num-curves)))
;(rand-intervals 4 10)
;([0 4] [2 4] [0 2] [0 3] [0 4] [0 3] [2 4] [0 2] [1 3] [2 4])

(defn subcurves
  "base of a sample curve, create num-curves that span random continous
  intervals of the original one"
  ([sample num-curves span]
   (let [length     (count sample)
         intervals  (rand-intervals length num-curves span)]
     (map #(apply subvec sample %) intervals)))
  ([sample num-curves]
   (subcurves sample num-curves 2)))

;(subcurves [[1 2] [3 4] [5 6] [7 8]] 3)
;([[5 6] [7 8]]
; [[1 2] [3 4] [5 6] [7 8]]
; [[3 4] [5 6]])

; TODO add noise function
; TODO add burst high noise function
; TODO randon curves

