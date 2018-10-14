(ns hiposfer.kamal.network.road
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is deftest]]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.services.routing.core :as router]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.specs.directions :as dataspecs]
            [hiposfer.kamal.services.routing.directions :as dir]
            [expound.alpha :as expound]))

(defonce network (delay (time (router/network {:area/edn "resources/test/frankfurt.edn.gz"}))))

(defspec ^:integration routing-directions
  30; tries -> expensive test
  (let [graph    (deref (deref network)) ;; delay atom
        nodes    (alg/nodes graph)
        gc       (count nodes)]
    (prop/for-all [i (gen/large-integer* {:min 0 :max (Math/ceil (/ gc 2))})]
      (let [src      (dir/->coordinates (:node/location (nth nodes i)))
            dst      (dir/->coordinates (:node/location (nth nodes (* 2 i))))
            depart   (gen/generate (s/gen ::dataspecs/departure))
            args     {:coordinates [src dst] :departure depart :steps true}
            result   (dir/direction graph args)]
        (if (nil? result)
          (do (println "no path found")
              (is (nil? result) "WTH?"))
          (is (s/valid? ::dataspecs/directions result)
              (str (expound/expound-str ::dataspecs/directions result))))))))

#_(clojure.test/run-tests)
