(ns hiposfer.kamal.router.geometry-test
  (:require [clojure.test :refer :all]
            [hiposfer.kamal.router.util.geometry :as geometry]))

(defn round
  [precision ^Double number]
  (let [p (Math/pow 10 precision)]
    (/ (Math/round (* number p)) p)))

(deftest bearing
  (is (= (round 5 (geometry/bearing [8.645333, 50.087314]
                                    [8.635897, 50.104172]))
         (round 5 (- 340.2517643511778 360))))
  (is (= (round 5 (geometry/bearing [-5.4253, 50.0359]
                                    [-3.0412 58.3838]))
         (round 5 8.522027775282766))))

(deftest angle
  (is (= (geometry/angle  20.0 45.0) 25.0))
  (is (= (geometry/angle -45.0 45.0) 90.0))
  (is (= (geometry/angle -85.0 90.0) 175.0))
  (is (= (geometry/angle -95.0 90.0) -175.0))
  (is (= (geometry/angle -45.0 125.0) 170.0)))

;(clojure.test/run-tests)
