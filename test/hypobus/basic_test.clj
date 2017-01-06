(ns hypobus.basic-test
  (:require [clojure.test :as t]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.spec.test :as stest]
            [frechet-dist.core :as frechet]
            [frechet-dist.protocols :as frepos]
            [frechet-dist.sampler :as sampler]
            [hypobus.basics.geometry :as geo]
            [hypobus.conjectures.core :as hypo]
            [hypobus.conjectures.specs :as hypec]
            [hypobus.conjectures.route :as route]
            [hypobus.simulation.faker :as faker]))


;; 06.01.2017
;; https://gist.github.com/kennyjwilli/8bf30478b8a2762d2d09baabc17e2f10
;; macro defined in order to use the instrumented functions in a test suite
(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   (when t/*load-tests*
     `(def ~(vary-meta name assoc :test `(fn []
                                           (let [check-results# (clojure.spec.test/check ~sym-or-syms ~opts)
                                                 checks-passed?# (every? nil? (map :failure check-results#))]
                                             (println check-results#)
                                             (if checks-passed?#
                                               (t/do-report {:type    :pass
                                                             :message (str "Generative tests pass for "
                                                                           (string/join ", " (map :sym check-results#)))})
                                               (doseq [failed-check# (filter :failure check-results#)
                                                       :let [r# (clojure.spec.test/abbrev-result failed-check#)
                                                             failure# (:failure r#)]]
                                                 (t/do-report
                                                   {:type     :fail
                                                    :message  (with-out-str (clojure.spec/explain-out failure#))
                                                    :expected (->> r# :spec rest (apply hash-map) :ret)
                                                    :actual   (if (instance? Throwable failure#)
                                                                (throw failure#)
                                                                (:clojure.spec.test/val failure#))})))
                                             checks-passed?#)))
        (fn [] (t/test-var (var ~name)))))))

(defspec-test test-haversine   `hypobus.basics.geometry/haversine {:clojure.spec.test.check/opts {:num-tests 50}})
(defspec-test test-recombine   `hypobus.conjectures.core/recombine {:clojure.spec.test.check/opts {:num-tests 50}})
(defspec-test test-hypothize   `hypobus.conjectures.core/hypothize {:clojure.spec.test.check/opts {:num-tests 50}})
(defspec-test test-conjectures `hypobus.conjectures.core/conjectures {:clojure.spec.test.check/opts {:num-tests 50}})

;************************ MANUALLY WRITTEN TEST DATA **************************;
;; data manually created based on a mapbox route created manually
(def connatra-trace [[-75.578556,6.254853],[-75.586194,6.258479],[-75.584177,6.262275],[-75.586495,6.265091],[-75.587568,6.270167],[-75.5904,6.272129],[-75.590271,6.272727],[-75.591173,6.272513],[-75.592546,6.27038],[-75.59452,6.27294],[-75.593919,6.273537],[-75.593147,6.273324],[-75.594906,6.27085],[-75.59658,6.264238]])

(def max-dist 40); meters
(def num-subcurves 50) ; amount of subcurves
(def min-span 0.3); minimum percentage of the original curve
(def normal-noise 0.00025); degrees. Standard deviation of the location
(def burst-noise 0.01); degrees. standard deviation of the burst noise
(def num-vandals 3)

; (def coordinates (sampler/refine connatra-trace max-dist frepos/distance))
; (def hypocurve   (mapv (fn [[lon lat]] (geo/->HypoPoint lon lat geo/MIN-WEIGHT geo/MAX-DISTRUST))
;                     coordinates))
; (def subcurves (faker/subcurves hypocurve num-subcurves min-span))
; (def noisy-curves (map #(faker/add-noise normal-noise %) subcurves))
; (def spam-curves  (faker/bandal-curves num-vandals coordinates))
; (def burst-curves (faker/add-burst burst-noise (last noisy-curves)))
; (def traces (shuffle (concat (butlast noisy-curves) spam-curves (vector burst-curves))))
;
; (def foo (sort-by hypo/avg-distrust (hypo/conjectures traces)))
; (frechet/distance hypocurve (nth foo 0))
; ;; TODO: make a real test using the test data
; (require '[proto-repl-charts.charts])
; (require '[proto-repl-charts.table])
;
; (proto-repl-charts.charts/custom-chart
;   "hypos"
;   {:data {:xs {"hypo-lat" "hypo-lon"
;                "lat" "lon"};; y x axis
;           :columns [(cons "hypo-lat" (map :lat (second (nth foo 0))))
;                     (cons "hypo-lon" (map :lon (second (nth foo 0))))
;                     (cons "lat" (map second coordinates))
;                     (cons "lon" (map first coordinates))]
;           :type "scatter"}})

(t/deftest connatra
  (let [coordinates  (sampler/refine connatra-trace max-dist frepos/distance)
        hypocurve    (mapv (fn [[lon lat]] (geo/->HypoPoint lon lat geo/MIN-WEIGHT geo/MAX-DISTRUST))
                           coordinates)
        subcurves    (faker/subcurves hypocurve num-subcurves min-span)
        noisy-curves (map #(faker/add-noise normal-noise %) subcurves)
        spam-curves  (faker/bandal-curves num-vandals coordinates)
        burst-curves (faker/add-burst burst-noise (last noisy-curves))
        traces       (shuffle (concat (butlast noisy-curves)
                                      spam-curves
                                      (vector burst-curves)))
        result    (sort-by hypo/avg-distrust (hypo/conjectures traces))
        best-hypo (first result)
        fredis    (frechet/partial-distance hypocurve best-hypo)
        match     (route/overlap hypocurve best-hypo (:couple fredis))]
   (t/is (> route/MAX-DISIM (/ (:dist fredis) match)))
   (t/is (>= match 0.8)))) ;; more than 80% overlap
