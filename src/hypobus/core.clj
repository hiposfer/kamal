(ns hypobus.core
  (:require [hypobus.util :as tool]
            [hypobus.route :as route]
            [hypobus.mapbox :as mapbox]
            [hypobus.basics.geometry :as geo]
            [hypobus.visuals.plotter :as plotter]
            [hypobus.simulation.data-handler :as sim]
            [clojure.data :refer [diff]]
            [clojure.core.matrix.stats :as stats]
            [clojure.core.reducers :as red])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:const ^:private THREAD-GROUP 20)

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

; ================== NORMAL CORE FUNCTIONS ====================;

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
  processing. For online processing prefer check-hypos"
  ([] (vector))
  ([traces]
   (if (empty? traces) traces
     (recombine traces)))
  ([tr1 tr2]
   (let [traces  (concat tr1 tr2)
         rtraces (remove-untrusted traces)]
     (if (= traces rtraces)
       (conjectures traces)
       (conjectures (remove-outliers rtraces))))))

; ===================================================================
;             SIMULATION RELATED FUNCTIONS
;====================================================================

(defn simulate-journey
  [filename & journeys]
  (newline)
  (println "---- file read started")
  (let [data-points  (time (sim/fetch-journeys filename journeys))
        _            (println "---- file read ended")
        trajectories (sim/organize-journey data-points)
        _            (println "---- parallel processing")
        result       (red/fold THREAD-GROUP conjectures check-hypos trajectories)]
    (println "---- done")
    (map (partial geo/tidy 20 100 geo/haversine) result)))

(defn simulate-day
  [filename]
  (println "[FILE] read started")
  (let [data-points (time (sim/fetch-all filename))]
    (println "[FILE] data fetched")
    (for [[jid points] (group-by sim/journey-id data-points)
      :when (not= jid "EMPTY-ID")
      :let [_           (println "---- organizing journey: " jid)
            traces      (sim/organize-journey points)
            _           (println "---- parallel processing")
            pre-result  (red/fold THREAD-GROUP conjectures check-hypos traces)
            _           (println "---- finalizing hypothesis")
            result      (remove-outliers (map (partial geo/tidy 20 100 geo/haversine) pre-result))
            best-result (first (sort-by avg-distrust result))]]
      (do (mapbox/write-geojson (str "assets/" jid ".geojson") best-result)
          (println "DONE !! with: " jid "\n")
          (newline)
          (System/gc)
          (Thread/sleep 10000)))))

; ===================================================================
;                             MAIN
;====================================================================

(defn -main
  ([]
   (simulate-day "resources/dublin/siri.20130116.csv")))

(defonce trajectories (sim/organize-journey
                        (sim/fetch-journeys
                          "resources/dublin/siri.20130116.csv"
                          ["00070001"])))

(count trajectories)

(System/gc)
(def tmp (time (sort-by avg-distrust
    (red/fold THREAD-GROUP conjectures check-hypos trajectories))))
(count tmp)
(System/gc)

;; (def foo (time (sort-by avg-distrust (conjectures trajectories))))
;; (count foo)

; TODO: create a mapbox to poly-line function

;; (def foo (sort-by avg-distrust
;;      (time (simulate-journey "resources/dublin/siri.20130116.csv" "00070001"))))
;; (count foo)

(map-indexed vector (map avg-distrust tmp))
(plotter/show-polyline (nth tmp 1))

(System/gc)

;; (mapbox/write-geojson "assets/00070001.geojson" (nth foo 0))
