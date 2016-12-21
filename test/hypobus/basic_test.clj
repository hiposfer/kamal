(ns hypobus.basic-test
  (:require [clojure.test :as test]
            [frechet-dist.core :as frechet]
            [frechet-dist.sampler :as sampler]
            [hypobus.basics.geometry :as geo]
            [hypobus.conjectures.core :as hypo]
            [hypobus.simulation.faker :as sim]))

;******************************* TEST DATA *******************************;
;; data manually created based on a mapbox route created manually
(def connatra-trace [[-75.578556,6.254853],[-75.586194,6.258479],[-75.584177,6.262275],[-75.586495,6.265091],[-75.587568,6.270167],[-75.5904,6.272129],[-75.590271,6.272727],[-75.591173,6.272513],[-75.592546,6.27038],[-75.59452,6.27294],[-75.593919,6.273537],[-75.593147,6.273324],[-75.594906,6.27085],[-75.59658,6.264238]])

(def max-dist 40); meters
(def num-subcurves 10) ; amount of subcurves
(def min-span 0.3); minimum percentage of the original curve
(def normal-noise 0.0001); degrees. Standard deviation of the location
(def burst-noise 0.01); degrees. standard deviation of the burst noise
(def num-vandals 1)

(def coordinates (sampler/refine connatra-trace max-dist geo/vhaversine))
(def subcurves (sim/subcurves coordinates num-subcurves min-span))
(def geo-subcurves (map (fn [s] (map #(zipmap [:lon :lat] %) s))
                        subcurves))

(def noisy-curves (map #(sim/add-noise normal-noise %) geo-subcurves))
(def spam-curves  (sim/bandal-curves num-vandals coordinates))
(def burst-curves (sim/add-burst burst-noise (last noisy-curves)))
(def traces (shuffle (concat (butlast noisy-curves) spam-curves (vector burst-curves))))

;; TODO: make a real test using the test data
; (def foo (hypo/recombine traces))
; (map (fn [curve] (frechet/frechet-dist (map #(apply geo/point %) connatra-trace)
;                                        curve
;                                        geo/haversine))
;     foo)

(test/deftest hypothize
  (let [connatra-trace [[-75.578556,6.254853],[-75.586194,6.258479],[-75.584177,6.262275],[-75.586495,6.265091],[-75.587568,6.270167],[-75.5904,6.272129],[-75.590271,6.272727],[-75.591173,6.272513],[-75.592546,6.27038],[-75.59452,6.27294],[-75.593919,6.273537],[-75.593147,6.273324],[-75.594906,6.27085],[-75.59658,6.264238]]
        max-dist      40; meters
        num-subcurves 10 ; amount of subcurves
        min-span      0.3; minimum percentage of the original curve
        normal-noise  0.0001; degrees. Standard deviation of the location
        burst-noise   0.01; degrees. standard deviation of the burst noise
        num-vandals   1

        coordinates   (sampler/refine connatra-trace max-dist geo/vhaversine)
        subcurves     (sim/subcurves coordinates num-subcurves min-span)
        geo-subcurves (map (fn [s] (map #(zipmap [:lon :lat] %) s))
                           subcurves)

        noisy-curves  (map #(sim/add-noise normal-noise %) geo-subcurves)
        spam-curves   (sim/bandal-curves num-vandals coordinates)
        burst-curves  (sim/add-burst burst-noise (last noisy-curves))
        traces        (shuffle (concat (butlast noisy-curves)
                                       spam-curves
                                       (vector burst-curves)))]
   (test/is (not (nil? traces))))
 (test/is (= 4 (+ 2 2)))
 (test/is (= 7 (+ 3 4))))

(test/run-tests 'hypobus.basic-test)
