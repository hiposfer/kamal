(ns sagacious-woof.curve
  (:require [sagacious-woof.util :refer [haversine]]
            [clojure.core.matrix :refer [add mul div]]
            [frechet-dist.core :refer [partial-frechet-dist]]

            [frechet-dist.sampler :refer [refine]]))

(defn- interpoint
  "computes the weighted interpolation of points p1 and p2 according to weights
  w1 and w2. returns a vector of [new-weight new-point]"
  [[f1 p1] [f2 p2]]
  (vector (+ f1 f2)
          (div (add (mul p1 f1) (mul p2 f2))
               (add f1 f2))))

(defn make-path
  "interpolate two curves A, B (each one with an implicit weight) following the
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
  "based on the coupling of two curves A and B, returns the maximum percentage
  of either A or B that matches the other curves (percentage as [0,1])"
  [lenght-A lenght-B coupling]
  (let [[Ao Bo]         (first coupling)
        [Az Bz]         (last coupling)
        pA              (/ (- Az Ao) lenght-A); percentage of A
        pB              (/ (- Bz Bo) lenght-B)]
    (max pA pB)))

(defn full-coupling
  "base on the curve A and on its partial coupling sequence with a curve B create a
  complete coupling sequence using A as a model"
  [length coupling]
  (let [co-start     (ffirst coupling)
        co-end       (first (last coupling))
        part1        (for [i (range (- co-start 0))]    [i nil])
        part3        (for [j (range (- co-end length))] [j nil])]
    (concat part1 coupling part3)))

(defn compare-curves
  "compare two curves P and Q. returns their partial-frechet-distance, their
  partial coupling sequence and their similarity"
  [P Q]
  (let [[dist coupling] (partial-frechet-dist (mapv second P)
                                              (mapv second Q)
                                              haversine)
        similarity      (check-similarity (dec (count P))
                                                (dec (count Q))
                                                coupling)]
    [dist coupling similarity]))

(defn merge-curves
  "merge curve hypo(thesis) with asample curve using their partial coupling
  sequence"
  [hypo asample coupling]
  (let [full-coupling   (full-coupling (dec (count hypo)) coupling)]
    (make-path hypo asample full-coupling)))

(def coordinates
            [[-75.578556,6.254853],
            [-75.586194,6.258479],
            [-75.584177,6.262275],
            [-75.586495,6.265091],
            [-75.587568,6.270167],
            [-75.5904,6.272129],
            [-75.590271,6.272727],
            [-75.591173,6.272513],
            [-75.592546,6.27038],
            [-75.59452,6.27294],
            [-75.593919,6.273537],
            [-75.593147,6.273324],
            [-75.594906,6.27085],
            [-75.59658,6.264238]])

(refine coordinates 0.2 haversine)

