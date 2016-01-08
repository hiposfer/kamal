(ns sagacious-woof.curve
  (:require [sagacious-woof.util :refer [haversine]]
            [clojure.core.matrix :refer [add mul div]]
            [frechet-dist.core :refer [partial-frechet-dist]]))

(defn- interpoint
  "computes the weighted interpolation of points p1 and p2 according to weights w1 and w2"
  [[f1 p1] [f2 p2]]
  (vector (+ f1 f2)
          (div (add (mul p1 f1) (mul p2 f2))
               (add f1 f2))))

(defn make-path
  "interpolate two curves A, B using the weights Wa, Wb and following the
  given coupling. Note that the coupling MUST contain a mapping of the complete
  curve to interpolate"
  [A B coupling]
  (into (vector)
    (for [[i j] coupling]
      (cond
       (nil? i) (nth B j)
       (nil? j) (nth A i)
       :else (interpoint (nth A i) (nth B j))))))

(defn check-similarity
  "compute the similarity between A and B using the partial frechet distance
  and the percentage of the curve that was coupled"
  [lenght-A lenght-B coupling]
  (let [[Ao Bo]         (first coupling)
        [Az Bz]         (last coupling)
        pA              (/ (- Az Ao) lenght-A); percentage of A
        pB              (/ (- Bz Bo) lenght-B)]
    (max pA pB)))
  ;(/ dist (max pA pB))

(defn full-coupling
  "base on the curve A and on its coupling sequence with a curve B create a
  complete coupling sequence using A as a model"
  [length coupling]
  (let [co-start     (ffirst coupling)
        co-end       (first (last coupling))
        part1        (for [i (range (- co-start 0))]    [i nil])
        part3        (for [j (range (- co-end length))] [j nil])]
    (concat part1 coupling part3)))

(defn compare-curves
  [P Q]
  (let [[dist coupling] (partial-frechet-dist (mapv second P)
                                              (mapv second Q)
                                              haversine)
        similarity      (check-similarity (dec (count P))
                                                (dec (count Q))
                                                coupling)]
    [dist coupling similarity]))
