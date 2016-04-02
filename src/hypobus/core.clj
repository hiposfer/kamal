(ns hypobus.core
  (:require [hypobus.util :as tool]
            [hypobus.route :as route]
            [hypobus.mapbox :as mapbox]
            [hypobus.basics.geometry :as geo]
            [hypobus.simulation.data-handler :as sim]
            [hypobus.visuals.plotter :as plotter])
  (:gen-class))

(defn check-trace
  "merge the new-curve with any similar hypothesis."
  [hypos trace]
  (for [hypo hypos
    :let [result (route/similarity hypo trace)]]
    (if-not (:similar? result) hypo
      (route/fuse hypo trace (:couple result)))))

(defn reduce-hypos
  "compare all possible combinations of hypothesis:
  - merge them if they match
  - keep them otherwise"
  [hypos]
;;   (println "hypothesis: " (count hypos))
  (if (empty? (rest hypos)) hypos
    (let [new-hypos (check-trace (rest hypos) (first hypos))]
      (if (= new-hypos (rest hypos))
        (conj (reduce-hypos (rest hypos)) (first hypos))
        (recur new-hypos)))))

(defn check-hypos
  "compare the newly arrived curve with the current hypothesis and merge them
  if necessary"
  [hypos trace]
;;   (println "hypothesis: " (count hypos))
  (let [new-hypos    (check-trace hypos trace)]
    (if (= new-hypos hypos)
      (conj new-hypos trace)
      (reduce-hypos new-hypos))))

(defn conjecture
  [traces]
  (reduce check-hypos [(first traces)] (rest traces)))

; ===================================================================
;             SIMULATION RELATED FUNCTIONS
;====================================================================

(defn simulate-journey
  [filename & journeys]
  (newline)
  (println "[FILE] read started")
  (let [data-points  (time (sim/fetch-journeys filename journeys))
        _ (println "[FILE] data fetched")
        trajectories (sim/organize-journey data-points)
        _ (println "\n[START] parallel processing")
        pre-result  (tool/pbatch conjecture trajectories)]
    (println "\n[END] parallel processing !")
    (map #(geo/tidy 20 100 geo/haversine %)
         (conjecture pre-result))))

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
                                (conjecture pre-result))
                best-result (first (reverse (sort-by tool/general-trust result)))]]
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
  ;([filename]
   ;(simulate-journey filename "00070001"))
  ;([filename & journeys]
   ;(simulate-journey filename journeys)))

; TODO: create a mapbox-feature to poly-line function

;; (defonce co (sim/fetch-line "resources/dublin/siri.20130116.csv" "9"))
;; (map count (sim/organize-journey co))

;; (def foo (reverse (sort-by tool/general-trust
;;      (time (simulate-journey "resources/dublin/siri.20130116.csv" "00070001")))))
;; (count foo)
;; (map-indexed vector (map tool/general-trust foo))

;; (plotter/show-polyline (nth foo 0))

;; (map geo/distance (nth foo 0) (rest (nth foo 0)))

