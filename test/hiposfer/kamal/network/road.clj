(ns hiposfer.kamal.network.road
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hiposfer.kamal.specs.directions :as dataspecs]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.services.routing.core :as router]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hiposfer.kamal.services.routing.directions :as dir]
            [hiposfer.kamal.services.routing.graph :as graph]))

(defonce network (delay (time (router/network {:area/edn "resources/test/frankfurt.edn.gz"}))))

(defspec ^:integration routing-directions
  30; tries -> expensive test
  (let [conn  (deref network) ;; force
        nodes (alg/nodes @conn)
        gc    (count nodes)
        graph (graph/create @conn)]
    (alter-meta! conn assoc :area/graph graph)
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
