(ns sagacious-woof.route
  (:require [sagacious-woof.util :as tools]
            [frechet-dist.core :refer [partial-frechet-dist]]))

(defn check-similarity
  "based on the coupling of two curves A and B, returns the maximum percentage
  of either A or B that matches the other curves (percentage as [0,1])"
  [lenght-P lenght-Q coupling]
  (let [[Po Qo]         (first coupling)
        [Pz Qz]         (last coupling)
        percP           (/ (- Pz Po) lenght-P); percentage of A
        percQ           (/ (- Qz Qo) lenght-Q)]
    (max percP percQ)))

(defn compare-curves
  "compare two curves P and Q. returns their partial-frechet-distance, their
  partial coupling sequence and their similarity"
  [geo-P geo-Q]
  (let [frechet     (partial-frechet-dist geo-P geo-Q tools/haversine)
        similarity  (check-similarity (tools/last-index geo-P)
                                      (tools/last-index geo-Q)
                                      (:couple frechet))]
    (into frechet {:sim similarity})))

(defn- mirror [vec-coll] (map (comp vec rseq) vec-coll))

(defn- containment
  [length-A coupling]
  (concat (for [i (range (first (first coupling)))]         [i nil])
          coupling
          (for [j (range (inc (first (last coupling))) length-A)] [j nil])))

(defn- overlap
  [length-B coupling]
  (concat (for [i (range (first (first coupling)))]         [i nil])
          coupling
          (for [j (range (inc (second (last coupling))) length-B)] [nil j])))

(defn full-coupling
  "base on the curve A and on its partial coupling sequence with a curve B create a
  complete coupling sequence using A as a model"
  [geo-P geo-Q coupling]
  (let [[fst-P fst-Q]   (first coupling)
        [lst-P lst-Q]   (map - (map tools/last-index [geo-P geo-Q])
                               (last coupling))]
    (cond
     ; Q c P
     (and (>= fst-P fst-Q) (>= lst-P lst-Q)) (containment (count geo-P) coupling)
     ; P c Q
     (and (>= fst-Q fst-P) (>= lst-Q lst-P)) (mirror (containment (count geo-Q) (mirror coupling)))
     ; P matches Q at Q's beginning
     (and (>= fst-P fst-Q) (>= lst-Q lst-P)) (overlap (count geo-Q) coupling)
     ; P matches Q at Q's end
     (and (>= fst-Q fst-P) (>= lst-P lst-Q)) (mirror (overlap (count geo-P) (mirror coupling))))))

(defn- interpoint
  "computes the weighted interpolation of points p1 and p2 according to weights
  w1 and w2. returns a vector of [new-weight new-point]"
  [p1 p2]
  {:w    (+ (:w p1) (:w p2))
   :lat  (/ (+ (* (:lat p1) (:w p1)) (* (:lat p2) (:w p2)))
            (+ (:w p1) (:w p2)))
   :long (/ (+ (* (:long p1) (:w p1)) (* (:long p2) (:w p2)))
            (+ (:w p1) (:w p2)))})

(defn make-path
  "interpolate two curves A, B (each one with an implicit weight) following the
  given coupling. Note that the coupling MUST contain a mapping of the complete
  curve to interpolate"
  [geo-P geo-Q coupling]
  (for [[i j] coupling]
    (cond
     (nil? i) (nth geo-Q j)
     (nil? j) (nth geo-P i)
     :default (interpoint (nth geo-P i) (nth geo-Q j)))))

(defn merge-curves
  "merge curve hypo(thesis) with asample curve using their partial coupling
  sequence"
  [geo-P geo-Q coupling]
  (let [mod-coupling   (full-coupling geo-P geo-Q coupling)]
    (make-path geo-P geo-Q mod-coupling)))


;(def C1 '([1 [-75.58898400000001 6.271148]] [1 [-75.5904 6.272129]] [1 [-75.590271 6.272727]] [1 [-75.591173 6.272513]] [1 [-75.5918595 6.2714465]] [1 [-75.592546 6.27038]] [1 [-75.59353300000001 6.271660000000001]] [1 [-75.59452 6.27294]] [1 [-75.593919 6.273537]] [1 [-75.593147 6.273324]] [1 [-75.5940265 6.272087]] [1 [-75.594906 6.27085]] [1 [-75.5953245 6.269197]] [1 [-75.595743 6.267544]]))
;(def hyp2 [[1 [-75.58552166666666 6.259744333333334]] [1 [-75.58484933333334 6.261009666666666]] [1 [-75.584177 6.262275]] [2 [-75.58494966666666 6.263213666666666]] [2 [-75.58572233333334 6.2641523333333335]] [2 [-75.586495 6.265091]] [3 [-75.58685266666666 6.266783]] [3 [-75.58721033333333 6.268475]] [3 [-75.587568 6.270166999999999]] [2 [-75.58898400000001 6.271148]] [2 [-75.5904 6.272129]] [1 [-75.590271 6.272727]] [1 [-75.591173 6.272513]] [1 [-75.5918595 6.2714465]]])

;(compare-curves hyp2 C1)
;(full-coupling hyp2 C1 (second (compare-curves hyp2 C1)))
