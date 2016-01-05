(ns sagacious-woof.simulator
  (:require [clojure.test.check.generators :as gen]))

(defn- rand-intervals
  [length num-curves]
  (let [start      (gen/choose 0 length)
        end        (gen/choose 0 length)
        interval   (gen/such-that (fn [[a b]] (>= (- b a) 2))
                                  (gen/tuple start end))]
    (gen/sample interval num-curves)))

(defn subcurves
  [sample num-curves]
  (let [length     (count sample)
        intervals  (rand-intervals length num-curves)]
    (map #(apply subvec sample %) intervals)))
