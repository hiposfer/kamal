(ns hypobus.conjectures.simulation.comparisons
  (:require [clojure.data :refer [diff]]
            [clojure.core.reducers :as red]
            [hypobus.utils.tool :as tool]
            [hypobus.basics.geometry :as geo]
            [hypobus.conjectures.route :as route]
            [hypobus.conjectures.simulation.data-handler :as sim]))
            ;[hypobus.visuals.plotter :as plotter]

(defn- comp-dist
  [geo-dpath shape-fname]
  (let [curves      (sim/geocurves geo-dpath)
        shapes      (sim/geoshapes shape-fname)
        comparables (diff (set (keys curves)) (set (keys shapes)))]
    (println "only in curves: " (first comparables))
    (println "only in shapes: " (second comparables))
    (for [id (nth comparables 2)
          :let [tid-shape (geo/tidy geo/haversine (get shapes id))]]
      {:id id :dist (min (:dist (geo/distance (get curves id) tid-shape))
                         (:dist (geo/distance (reverse (get curves id)) tid-shape)))})))

;; (defonce tmp (comp-dist "/home/camilo/Proyectos/hypobus/assets/filtered/" "assets/gtfs/shapes.txt"))

;; (sort-by first (map (fn [[k v]] [k (count v)])
;;   (group-by #(* (quot (:dist %) 20) 20) tmp)))
;; tmp

;; (with-open [out-file (clojure.java.io/writer "assets/distances.csv")]
;;   (clojure.data.csv/write-csv out-file (map vals tmp)))

;; (defonce curves (sim/geocurves "/home/camilo/Proyectos/hypobus/assets/filtered/"))
;; (defonce shapes (sim/geoshapes "assets/gtfs/shapes.txt"))

;; (def c9 (get curves "9"))
;; (def s9 (get shapes "9"))

;(incanter.core/view
;; (plotter/plot
;;   (chart/scatter-plot (concat (map :lon c9) (map :lon s9))
;;                       (concat  (map :lat c9) (map :lat s9))
;;               :group-by (concat (repeat (count c9) "curve") (repeat (count s9) "shape"))
;;               :x-label "longitude"
;;               :y-label "latitude"))
;(plotter/show-polyline (get shapes "9"))

;(incanter.core/view
;; (plotter/plot
;;   (chart/histogram (remove #(> % 5000) (map :dist tmp)) :nbins 20 :density false :x-label "distance"))
;;   ;(chart/set-axis :x (chart/log-axis))))
;; ;(incanter.core/view
;; ;; (plotter/plot
;; ;;   (chart/histogram (reductions + (map :dist (sort-by :dist tmp))) :nbins 100 :density true))

;; (double (/ (count (remove #(< % 2000) (map :dist tmp))) (count tmp)))

;(mapbox/write-geojson "assets/shape-9.geojson" (get shapes "9"))


;; (let [data-points (time (sim/fetch-all "resources/dublin/siri.20130116.csv"))]
;;   (println "[FILE] data fetched")
;;   (for [[jid points] (group-by sim/journey-id data-points)
;;         :when (not= jid "EMPTY-ID")
;;         :let [trajectories (vals (group-by :vehicle-journey-id points))
;;               raw (mapv #(mapv geo/point %) trajectories)]]
;;         (do (mapbox/write-multiline (str "assets/raw/" jid ".geojson") raw)
;;           (println "DONE !! with: " jid "\n")
;;           (newline)
;;           (System/gc))))
