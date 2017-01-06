(ns hypobus.conjectures.route
  "functions related to the comparison of two curves, as well as how to merge them"
  (:require [hypobus.utils.tool :as tool]
            [frechet-dist.core :as frechet]
            [frechet-dist.protocols :as frepos]
            [hypobus.basics.geometry :as geo]))

; damping factor
(def EPSILON 1.0)
; Maximum value admited to consider two curves similar
(def MAX-DISIM (/ 50.0 0.6)) ;; 50 meters with 60 % similarity

(defn arc-length
  "computes the partial arc-length as the sum of the point to point distances
  of a subcurve determined by the start, end limits"
  [p2p-dist start end]
  (reduce + (subvec p2p-dist start end)))

(defn overlap
  "returns the maximum percentage of either curve P or Q that was matched
  according to the partial coupling sequence (percentage as [0,1])"
  [P Q coupling]
  (let [[idx-p idx-q]    (first coupling)
        [jdx-p jdx-q]    (last coupling)
        dPij       (mapv frepos/distance P (rest P))
        dQij       (mapv frepos/distance Q (rest Q))
        percP      (/ (arc-length dPij idx-p jdx-p) ; percentage of A
                      (arc-length dPij 0 (tool/last-index P)))
        percQ      (/ (arc-length dQij idx-q jdx-q)
                      (arc-length dQij 0 (tool/last-index Q)))]
    (max percP percQ)))

(defn- mirror
  "reverses the order of each couple in the coupling sequence"
  [vcoll]
  (map (comp vec rseq) vcoll))

(defn- enclose
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

(defn- protrude
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
        [j_P j_Q]   (map - [(tool/last-index P) (tool/last-index Q)]
                           (last coupling))]
    (cond
     ; Q contained in P
     (and (>= i_P i_Q) (>= j_P j_Q)) (enclose (count P) coupling)
     ; P contained in Q
     (and (>= i_Q i_P) (>= j_Q j_P)) (mirror (enclose (count Q) (mirror coupling)))
     ; P matches Q at Q's beginning
     (and (>= i_P i_Q) (>= j_Q j_P)) (protrude (count Q) coupling)
     ; P matches Q at Q's end
     (and (>= i_Q i_P) (>= j_P j_Q)) (mirror (protrude (count P) (mirror coupling))))))

(defn fuse
  "fuse two objects into one. For points, it computes the weighted arithmetic
  mean and for Polygonal lines it fuses then by fusing each point on each curve
  with its equivalent according to the coupling sequence."
  ([p1 p2] ;"computes the weighted interpolation of points p1 and p2 according to weights w1 and w2"
   (let [{:keys [weight lat lon distrust]} p1
         {w2 :weight, lat2 :lat, lon2 :lon, dt2 :distrust} p2]
     (geo/->HypoPoint
       (/ (+ (* lon weight) (* lon2 w2)) (+ weight w2))
       (/ (+ (* lat weight) (* lat2 w2)) (+ weight w2))
       (+ weight w2)
       (/ 1.0 (+ (/ 1.0 distrust) (/ EPSILON dt2))))))
  ([P Q coupling] ; a polygonal line is just a collection of points
   (let [real-coupling (full-coupling P Q coupling)]
     (for [[i j] real-coupling]
       (cond
         (nil? i) (nth Q j)
         (nil? j) (nth P i)
         :else (fuse (nth P i) (nth Q j)))))))
