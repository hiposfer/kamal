(ns sagacious-woof.core
  (:require [sagacious-woof.broker :as broker]
            [sagacious-woof.curve :as curve]
            [sagacious-woof.simulator :as sim]
            [sagacious-woof.util :refer [combinations]]

            [incanter.charts :as chart]
            [incanter.core :as incanter])
  (:gen-class))

(defn good-match
  [index [dist coupling similarity]]
  (if (and (> similarity 0.7) (< dist 1))
    [index coupling] nil))

(defn merge-curves
  [hypo asample coupling]
  (let [full-coupling   (curve/full-coupling (dec (count hypo)) coupling)]
    (curve/make-path hypo asample full-coupling)))

(defn examinate-sample
  [hypotcs asample]
  (for [hypo hypotcs
    :let [[dist coupling similarity] (curve/compare-curves hypo asample)]]
    (cond
     ; similar curves
     (and (> similarity 0.7) (< dist 0.1)) (merge-curves hypo asample coupling)
     ; ambiguous situation
     ;(< 0.3 similarity 0.7) hypo ; TODO !!
     ;(comment merge hypo-curve with sample and sample with hypo-curve)
     ; dissimilar curves
     :default (concat hypo asample))))

(defn check-hypothesis
  [hypotcs asample]
  (let [new-hypos     (examinate-sample hypotcs asample)
        combi-pairs   (combinations 2 new-hypos)
        comparisons   (mapv #(apply curve/compare-curves %) combi-pairs)
        similars      (keep-indexed good-match comparisons)
        merged-hypos  (for [[index coupling] similars
                            :let [[P Q] (nth combi-pairs index)]]
                        (merge-curves P Q coupling))
        matched       (->> (map first similars)
                           (map #(nth combi-pairs %))
                           (apply concat)
                           (distinct)
                           (into (hash-set)))
        unmatched     (filter #(not (matched %)) new-hypos)]
    (concat matched unmatched)))

; TODO after a check up with the new curve is finish. Check if the hypotcs changed
; if so then compare the hypotcs themselves but only merge those that are very similar
(defn -main
  "simulate"
  [sample num-samples]
  (let [pcurves            (sim/subcurves sample num-samples)
        ;pcurves      (vector (take 10 coordinates) (take 5 coordinates) (take-last 10 coordinates) (take-last 12 coordinates))
        [fst-curve & nth-curves] (map broker/find-trust pcurves)]
    (reduce check-hypothesis [fst-curve] nth-curves)))

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

(def foo (-main coordinates 3))
(def bar (map second (first foo)))
(incanter/view (chart/scatter-plot (map first bar) (map second bar)))

;(incanter/view (chart/scatter-plot (map first coordinates) (map second coordinates)))
