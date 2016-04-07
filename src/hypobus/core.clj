(ns hypobus.core
  (:require [hypobus.util :as tool]
            [hypobus.route :as route]
            [hypobus.mapbox :as mapbox]
            [hypobus.basics.geometry :as geo]
            [hypobus.simulation.data-handler :as sim]
            [hypobus.visuals.plotter :as plotter]

            [clojure.core.matrix.stats :as stats])
  (:gen-class))

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
  (let [min-dist (apply min (map avg-distrust curves))]
    (if-not (< min-dist 0.1) curves
      (remove #(< 0.9 (avg-distrust %)) curves))))

(defn- remove-outliers
  "takes a sequence of curves and remove the points of each curves that are
  more than 3 standard deviations apart from the mean distrust of the curve.
  If the new curve doesn't have more than 3 points, it is also removed"
  [curves]
  (let [ncurves (for [curve curves
                  :let [dt    (avg-distrust curve)
                        sd-dt (sd-distrust curve)]]
                  (remove #(> (Math/abs (- (:distrust %) dt)) (* 3 sd-dt)) curve))]
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

(defn check-hypos
  "compare a trace with the current hypothesis. If any of them matches they are
  merged, otherwise returned as they are. In case a merge occurs, the new
  hypothesis are re-checked for any possible new match among them"
  [hypos trace] ;;   (println "hypothesis: " (count hypos))
  (let [new-hypos    (check-trace hypos trace)]
    (if (= new-hypos hypos)
      (conj hypos trace)
      (reduce check-hypos [(first new-hypos)] (rest new-hypos)))))

(defn conjecture
  "takes a sequence of traces and tries to reduce them by merging similar ones
  and keeping unique ones. This is mainly an utility function used for batch
  processing. In normal cases prefer the direct use of check-hypos"
  [traces]
  (reduce check-hypos [(first traces)] (rest traces)))

; ===================================================================
;             SIMULATION RELATED FUNCTIONS
;====================================================================

(defn simulate-journey
  [filename & journeys]
  (newline)
  (println "---- file read started")
  (let [data-points  (time (sim/fetch-journeys filename journeys))
        _ (println "---- file read ended")
        trajectories (sim/organize-journey data-points)
        _ (println "---- starting parallel processing")
        pre-result  (tool/pbatch conjecture trajectories)]
    (println "---- finalizing hypothesis")
    (map #(geo/tidy 20 100 geo/haversine %)
         (conjecture (remove-outliers (remove-untrusted pre-result))))))

(defn simulate-day
  [filename]
  (println "[FILE] read started")
  (let [data-points (time (sim/fetch-all filename))]
    (println "[FILE] data fetched")
    (for [[jid points] (group-by sim/journey-id data-points)
          :when (not= jid "EMPTY-ID")
          :let [_ (println "---- organizing journey: " jid)
                traces     (sim/organize-journey points)
                _ (println "---- starting parallel processing")
                pre-result (tool/pbatch conjecture traces)
                _ (println "---- finalizing hypothesis")
                result     (map #(geo/tidy 20 100 geo/haversine %)
                                (conjecture (remove-outliers (remove-untrusted pre-result))))
                best-result (first (sort-by avg-distrust result))]]
      (do (mapbox/write-geojson (str "assets/" jid ".geojson") best-result)
          (println "DONE !! with: " jid "\n")
          (newline)
          (System/gc)
          (Thread/sleep 60000)))))

; ===================================================================
;                             MAIN
;====================================================================

(defn -main
  ([]
   (simulate-day "resources/dublin/siri.20130116.csv")))

; TODO: create a mapbox to poly-line function

;; (defonce co (sim/fetch-line "resources/dublin/siri.20130116.csv" "9"))
;; (map count (sim/organize-journey co))

;; (def foo (sort-by avg-distrust
;;      (time (simulate-journey "resources/dublin/siri.20130116.csv" "00070001"))))
;; (count foo)
;; (map-indexed vector (map avg-distrust foo))
(System/gc)

;; (mapbox/write-geojson "assets/00070001.geojson" (nth foo 0))

;; (plotter/show-polyline (nth foo 0))
;; (map geo/distance (nth foo 0) (rest (nth foo 0)))

; USE CORE.MATRIX STATS LIBRARY FOR MEAN, DEVIATION AND MORE

