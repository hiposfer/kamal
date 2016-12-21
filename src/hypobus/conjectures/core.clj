(ns hypobus.conjectures.core
  (:require [clojure.core.matrix.stats :as stats]
            [hypobus.utils.tool :as tool]
            [hypobus.conjectures.route :as route]))

; ================== NOT NAMESPACED YET ====================;

(defn- avg-distrust
  "calculate the average distrust in a geo-curve"
  [curve]
  (stats/mean (map :distrust curve)))

(defn- sd-distrust
  "calculate the standard deviation of the trust in a geo-curve"
  [curve]
  (stats/sd (map :distrust curve)))

(defn- remove-untrusted
  "takes a sequence of curves and removes those whose average distrust exceeds
  0.9. The removal only happens in any one curve has an average distrust less
  than 0.1, otherwise all curves are returned as they are"
  [curves]
  (let [min-dt (apply min (map avg-distrust curves))]
    (if (< 0.1 min-dt) curves
      (remove #(< 0.9 (avg-distrust %)) curves))))

(defn- remove-outliers
  "takes a sequence of curves and remove the points of each curves that are
  more than 3 standard deviations apart from the mean distrust of the curve.
  If the new curve doesn't have more than 3 points, it is also removed"
  [curves]
  (let [ncurves (for [curve curves
                      :let [dt    (avg-distrust curve)
                            sd-dt (sd-distrust curve)]]
                  (remove #(> (Math/abs (double (- (:distrust %) dt))) (* 3 sd-dt)) curve))]
    (remove #(> 3 (count %)) ncurves)))

; ================== CORE FUNCTIONS ====================;

(defn check-trace
  "compare all the hypothesis with a given trace, if they match they are
  merged, otherwise they are returned as they are."
  [hypos trace]
  (for [hypo hypos
        :let [result (route/similarity hypo trace)]]
    (if-not (:similar? result) hypo
      (route/fuse hypo trace (:couple result)))))

(defn- with-similar
  "utility function to use inside recombine"
  [_ [c1 c2]]
  (let [res (route/similarity c1 c2)]
    (when (:similar? res)
      (reduced [c1 c2 res]))))

(defn recombine
  "takes a sequence of hypotheses and compares them all (all possible combinations).
  If any of those are similars, they are merge and the function recurs with
  the new hypotheses"
  [hypos]
  (let [combis                  (tool/combinations 2 hypos)
        [c1 c2 res :as a-match] (reduce with-similar nil combis)
        old-hypos               (some-> a-match (set) (remove hypos))]
    (if-not a-match hypos
      (recur (cons (route/fuse c1 c2 (:couple res)) old-hypos)))))

(defn hypothize
  "compare a trace with the current hypothesis. If any of them matches they are
  merged, otherwise returned as they are. In case a merge occurs, the new
  hypothesis are re-checked for any possible new match among them"
  [hypos trace] ;;   (println "hypothesis: " (count hypos))
  (let [new-hypos    (check-trace hypos trace)]
    (if (= new-hypos hypos)
      (conj hypos trace)
      (recombine new-hypos))))

(defn conjectures
  "takes a sequence of traces and tries to reduce them by merging similar ones
  and keeping unique ones. This function is meant to be used for parallel/batch
  processing. For online processing prefer hypothize"
  ([] (vector))
  ([traces] (recombine traces))
  ([tr1 tr2]
   (let [traces  (concat tr1 tr2)
         rtraces (remove-untrusted traces)]
     (if (= traces rtraces)
       (recombine traces)
       (recombine (remove-outliers rtraces))))))
