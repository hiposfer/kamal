(ns hypobus.simulation.core
  (:require [clojure.core.reducers :as red]
            [hypobus.utils.mapbox :as mapbox]
            [hypobus.basics.geometry :as geo]
            [hypobus.conjectures.core :as hypo]
            [hypobus.simulation.data-handler :as sim]
            [hypobus.simulation.visuals.plotter :as plotter]))

;; HACKING private vars into ns public ones
(def remove-outliers #'hypo/remove-outliers)
(def avg-distrust #'hypo/avg-distrust)

(def ^:const ^:private THREAD-GROUP 20)

(defn simulate-journey
  [filename & journeys]
  (newline)
  (println "---- file read started")
  (let [data-points  (time (sim/fetch-journeys filename journeys))
        _            (println "---- file read ended")
        trajectories (sim/organize-journey data-points)
        _            (println "---- parallel processing")
        result       (red/fold THREAD-GROUP hypo/conjectures hypo/hypothize trajectories)]
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
                pre-result  (red/fold THREAD-GROUP hypo/conjectures hypo/hypothize traces)
                _           (println "---- finalizing hypothesis")
                result      (remove-outliers (map (partial geo/tidy 20 100 geo/haversine) pre-result))
                best-result (first (sort-by avg-distrust result))]]
      (do (mapbox/write-geojson (str "assets/" jid ".geojson") best-result)
          (println "DONE !! with: " jid "\n")
          (newline)
          (System/gc)
          (Thread/sleep 1000)))))

;; (defonce trajectories (sim/organize-journey
;;                         (sim/fetch-journeys
;;                           "resources/dublin/siri.20130116.csv"
;;                           ["00070001"])))

;; (count trajectories)

;; (System/gc)
;; (def tmp (time (sort-by avg-distrust
;;     (red/fold THREAD-GROUP conjectures check-hypos trajectories))))
;; (count tmp)
;; (System/gc)

;; (def foo (time (sort-by avg-distrust (conjectures trajectories))))
;; (count foo)

; TODO: create a mapbox to poly-line function

;; (def foo (sort-by avg-distrust
;;      (time (simulate-journey "resources/dublin/siri.20130116.csv" "00070001"))))
;; (count foo)

;; (map-indexed vector (map avg-distrust tmp))
;; (plotter/show-polyline (nth tmp 1))

;; (simulate-day "resources/dublin/siri.20130116.csv")
;; (System/gc)

;; (mapbox/write-geojson "assets/00070001.geojson" (nth foo 0))
