(ns hiposfer.kamal.router.road
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hiposfer.kamal.server.specs.directions :as dataspecs]
            [hiposfer.kamal.router.generative :as rt]
            [hiposfer.kamal.router.core :as router]
            [hiposfer.kamal.router.directions :as dir]))

(defonce network (delay (time (router/network {:area/edn "resources/test/frankfurt.edn.gz"}))))

(defspec ^:integration routing-directions
  30; tries -> expensive test
  (let [conn  (deref network) ;; force
        nodes (rt/nodes @conn)
        gc    (count nodes)]
    (prop/for-all [i (gen/large-integer* {:min 0 :max (Math/ceil (/ gc 2))})]
      (let [src      (dir/->coordinates (:node/location (nth nodes i)))
            dst      (dir/->coordinates (:node/location (nth nodes (* 2 i))))
            depart   (gen/generate (s/gen ::dataspecs/departure))
            args     {:coordinates [src dst] :departure depart :steps true}
            result   (dir/direction conn args)]
        (if (nil? result)
          (do (println "no path found")
              (is (nil? result) "WTH?"))
          (is (s/valid? ::dataspecs/directions result)
              (str (expound/expound-str ::dataspecs/directions result))))))))

#_(clojure.test/run-tests)
