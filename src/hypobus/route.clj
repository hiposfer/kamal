(ns hypobus.route
  "functions related to the comparison of two curves, as well as how to merge them"
  (:require [hypobus.basics.geometry :as geo]
            [hypobus.util :refer [last-index]]
            [frechet-dist.core :refer [partial-frechet-dist]]))

; damping factor
(def ^:const ^:private EPSILON 1)
; Maximum value admited to consider two curves similar
(def ^:const ^:private MAX-DISIM (/ 50 0.6))

(defn- arch-length
  "computes the partial arch-length as the sum of the point to point distances
  of a subcurve determined by the start, end limits"
  [p2p-dist start end]
  (reduce + (subvec p2p-dist start end)))

(defn- overlap-perc
  "returns the maximum percentage of either curve P or Q that was matched
  according to the partial coupling sequence (percentage as [0,1])"
  [P Q coupling]
  (let [[idx-p idx-q]    (first coupling)
        [jdx-p jdx-q]    (last coupling)
        dPij       (mapv geo/distance P (rest P))
        dQij       (mapv geo/distance Q (rest Q))
        percP      (/ (arch-length dPij idx-p jdx-p) ; percentage of A
                      (arch-length dPij 0 (last-index P)))
        percQ      (/ (arch-length dQij idx-q jdx-q)
                      (arch-length dQij 0 (last-index Q)))]
    (max percP percQ)))

(defn similarity
  "compare two curves P and Q and returns their partial-frechet-distance, their
  partial coupling sequence and whether or not they are similar"
  [P Q]
  (let [frechet     (geo/distance P Q)
        perc-match  (overlap-perc P Q (:couple frechet))]
    (into frechet {:similar? (> MAX-DISIM (/ (:dist frechet) perc-match))})))

;(def similarity (memo/lru simple-similarity :lru/threshold 15))

(defn- mirror
  "reverses the order of each couple in the coupling sequence"
  [vcoll]
  (map (comp vec rseq) vcoll))

(defn- containment
  "extend the coupling sequence for the case of one curve being contained
  into the other. length is the amount of points in the polygonal line that
  contains the other line"
  [length coupling]
  (let [non-overlap-start (range (ffirst coupling))
        ; from the last overlaping point until the end of the curve
        non-overlap-end   (range (inc (first (last coupling))) length)]
  (concat (for [i non-overlap-start] [i nil])
          coupling
          (for [j non-overlap-end]   [j nil]))))

(defn- overlap
  "extend the coupling sequence for the case of one curve overlaping another
  either at its end or its beginning. length is the size of the curve that was
  compared against. "
  [length coupling]
  (let [non-overlap-start (range (ffirst coupling))
        ; from the last overlaping point until the end of the other curve
        non-overlap-end   (range (inc (second (last coupling))) length)]
  (concat (for [i non-overlap-start] [i nil])
          coupling
          (for [j non-overlap-end]   [nil j]))))

(defn full-coupling
  "takes the coupling of P with Q and returns a sequence of coupling
  pairs that can be used to merge both curves. Points that are in one curve
  but not on the other are returned as nil"
  [P Q coupling]
  (let [[i_P i_Q]   (first coupling)
        [j_P j_Q]   (map - [(last-index P) (last-index Q)]
                           (last coupling))]
    (cond
     ; Q contained in P
     (and (>= i_P i_Q) (>= j_P j_Q)) (containment (count P) coupling)
     ; P contained in Q
     (and (>= i_Q i_P) (>= j_Q j_P)) (mirror (containment (count Q) (mirror coupling)))
     ; P matches Q at Q's beginning
     (and (>= i_P i_Q) (>= j_Q j_P)) (overlap (count Q) coupling)
     ; P matches Q at Q's end
     (and (>= i_Q i_P) (>= j_P j_Q)) (mirror (overlap (count P) (mirror coupling))))))

(defn fuse
  "fuse two objects into one. For points, it computes the weighted arithmetic
  mean and for Polygonal lines it fuses then by fusing each point on each curve
  with its equivalent according to the coupling sequence."
  ([p1 p2] ;"computes the weighted interpolation of points p1 and p2 according to weights w1 and w2"
   (let [{:keys [weight lat lon distrust]}              p1
         {w2 :weight lat2 :lat lon2 :lon dt2 :distrust} p2]
     (geo/point
       (/ (+ (* lat weight) (* lat2 w2)) (+ weight w2)) ; :lat
       (/ (+ (* lon weight) (* lon2 w2)) (+ weight w2)) ; :lon
       (+ weight w2)                                    ; :weight
       (/ 1 (+ (/ 1 distrust) (/ EPSILON dt2))))))      ; :distrust
  ([P Q coupling] ; a polygonal line is just a collection of points
   (let [real-coupling (full-coupling P Q coupling)]
     (for [[i j] real-coupling]
       (cond
         (nil? i) (geo/point (nth Q j)) ; avoid holding on to the previous curve
         (nil? j) (geo/point (nth P i)) ; avoid holding on to the previous curve
         :else (fuse (nth P i) (nth Q j)))))))
