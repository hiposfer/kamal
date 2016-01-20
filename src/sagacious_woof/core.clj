(ns sagacious-woof.core
  (:require [sagacious-woof.broker :as broker]
            [sagacious-woof.route :as rout]
            [sagacious-woof.simulator :as sim]
            [sagacious-woof.util :refer [combinations]]

            [incanter.charts :as chart]
            [incanter.core :as incanter]

            [frechet-dist.core :as curve])
  (:gen-class))

; TODO: avoid magic numbers !!

(defn- freqs
  [weight-curve]
  (reduce + (map first weight-curve)))

(defn- good-match?
  [dist similarity]
  (and (> similarity 0.5) (< dist 0.2)))

(defn- matching-pair
  "check if the partial distance and similarity of two curves are under a certain
  threshold. If so return the index of the curve and their partial coupling"
  [index [dist coupling similarity]]
  (if (good-match? dist similarity)
    [index coupling] nil))

(defn examinate-sample
  "check if asample curve matches any of the current hypothesis curves, if so
  merge them, otherwise add asample curve to the set of hypothesis"
  [hypotcs asample]
  (let [new-hypos (for [hypo hypotcs
                  :let [[dist coupling similarity] (rout/compare-curves hypo asample)]]
                    (if (good-match? dist similarity)
                      (rout/merge-curves hypo asample coupling)
                      hypo))]
    (if (= new-hypos hypotcs) (conj new-hypos asample) new-hypos)))

(defn recombine
  [hypotcs]
  (let [curve-pairs   (combinations 2 hypotcs)
        comparisons   (mapv #(apply rout/compare-curves %) curve-pairs)
        similars      (keep-indexed matching-pair comparisons)
        merged-hypos  (for [[index coupling] similars
                            :let [[P Q] (nth curve-pairs index)]]
                        (rout/merge-curves P Q coupling))
        matched       (->> (map first similars)
                           (map #(nth curve-pairs %))
                           (apply concat)
                           (distinct)
                           (into (hash-set)))
        unmatched     (filter #(not (matched %)) hypotcs)
        new-hypos     (concat merged-hypos unmatched)]
    (if (not= new-hypos hypotcs)
      (recur new-hypos)
      hypotcs)))

(defn check-hypothesis
  [hypotcs asample]
  (let [new-hypos     (examinate-sample hypotcs asample)]
    (recombine new-hypos)))

; TODO after a check up with the new curve is finish. Check if the hypotcs changed
; if so then compare the hypotcs themselves but only merge those that are very similar
(defn -main
  "simulate"
  [pcurves]
  (let [[fst-curve & nth-curves] (map broker/find-trust pcurves)]
    (reduce check-hypothesis [fst-curve] nth-curves)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def coordinates [[-75.578556 6.254853] [-75.58008360000001 6.2555781999999995] [-75.58161120000001 6.2563034] [-75.5831388 6.2570286] [-75.5846664 6.257753800000001] [-75.586194 6.258479] [-75.58552166666666 6.259744333333334] [-75.58484933333334 6.261009666666666] [-75.584177 6.262275] [-75.58494966666666 6.263213666666666] [-75.58572233333334 6.2641523333333335] [-75.586495 6.265091] [-75.58685266666667 6.266783] [-75.58721033333333 6.268475] [-75.587568 6.270167] [-75.58898400000001 6.271148] [-75.5904 6.272129] [-75.590271 6.272727] [-75.591173 6.272513] [-75.5918595 6.2714465] [-75.592546 6.27038] [-75.59353300000001 6.271660000000001] [-75.59452 6.27294] [-75.593919 6.273537] [-75.593147 6.273324] [-75.5940265 6.272087] [-75.594906 6.27085] [-75.5953245 6.269197] [-75.595743 6.267544] [-75.5961615 6.265891] [-75.59658 6.264238]])
;pcurves      (vector (take 15 coordinates) (subvec coordinates 2 15) (subvec coordinates 2 20) (subvec coordinates 2 20))

(def pcurves (sim/subcurves coordinates 10 4))

(def foo (sort-by freqs > (-main pcurves)))
(count foo)

;(for [route pcurves]
;  (incanter/view (chart/scatter-plot (map first route) (map second route))))

(for [route (map #(map second %) foo)]
  (incanter/view (chart/scatter-plot (map first route) (map second route))))

;(incanter/view (chart/scatter-plot (map first coordinates) (map second coordinates)))
