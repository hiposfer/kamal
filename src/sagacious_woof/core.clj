(ns sagacious-woof.core
  (:require [sagacious-woof.distance :refer [haversine]]
            [frechet-dist.core :as curve])
  (:gen-class))

(defn- weighted-point
  "computes the weighted interpolation of points p1 and p2 according to weights w1 and w2"
  [w1 p1 w2 p2]
  (/ (+ (* p1 w1) (* p2 w2)) (+ w1 w2)))

(defn- interpolate-curve
  "interpolate two curves A, B using the weights Wa, Wb and following the
  given coupling. Note that the coupling MUST contain a mapping of the complete
  curve to interpolate"
  [A Wa B Wb coupling]
  (for [[i j] coupling]
    (cond
     (nil? i) (B j)
     (nil? j) (A i)
     :else (weighted-point (Wa i) (A i) (Wb j) (B j)))))

(defn how-similar
  "compute the similarity between A and B using the partial frechet distance
  and the percentage of the curve that was coupled"
  [A B]
  (let [[dist coupling] (curve/partial-frechet-dist A B haversine)
        [Ao Bo]         (first coupling)
        [Az Bz]         (last coupling)
        pA              (/ (- Az Ao) (dec (count A))); percentage of A
        pB              (/ (- Bz Bo) (dec (count B)))]
    (/ dist (max pA pB))))

(defn make-hypothesis
  "based on the similarity of two curves decide if they should be merged
  or not and how. returns a sequence of [pi ci] where pi is the probability
  that each point belongs to the real route"
  [hypotcs similarities A pa]
  (let [disimilar  (every? #(< % 0.3) similarities)
        similars   (keep-indexed #(if (> %2 0.7) (hypotcs %1) nil)
                                  similarities)
        ambiguous  (keep-indexed #(if (and (> %2 0.3) (< %2 0.7)) (hypotcs %1) nil)
                                  similarities)]
    (cond
     disimilar (conj hypotcs [pa A]) ; unique curve
     (not-empty similars) (comment merge each similar hypotcs with A) ; ignore the ambigous hypotcs
     (not-empty ambiguous) (comment merge each ambiguous hypotcs with A) ; ignore the very disimilar hypotcs
     :else "ERROR")))

(defn check-hypothesis
  "base on a sequence of current possible routes 'cresult' check if the new
  curve A confirms cresult or not and decide wether or not to create new
  beliefs based on it. returns a sequence of beliefs '[pi ci]'"
  [hypotcs [pa A]]
  (if (empty? hypotcs) [[pa A]]
    (let [similarities   (for [[po cmerge] hypotcs]
                           (comment compute-similarity with A))
          nresult        (comment check similarities and decide if A should be a
                           new belief or if it should be merge with a current belief)
          confidences    (comment compute each confidence)]
      (comment base on the confidence of each hypotcs increase its point
        probability and return the new hypotcs))))
      ; NOTE: once you have 'enough' confidence about your hypotcs, filter out
      ; those that are below a threshold

(defn -main
  "simulate"
  [sample]
  (let [pcurves         (comment partial-curves simulator)
        tcurves         (comment find out how much do I trust this sample)]
    (reduce (comment check-hypothesis) tcurves [])))

(def coordinates [[-75.578556,6.254853],
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
