(ns hiposfer.kamal.network.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.network.generators :as ng]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.services.routing.core :as router]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.network.tests :as kt]
            [hiposfer.kamal.network.road :as road]
            [datascript.core :as data]
            [hiposfer.kamal.services.routing.transit :as transit]
            [hiposfer.kamal.network.generators :as fake-area])
  (:import (java.time ZonedDateTime Duration LocalTime)))

;; NOTE: we put a letter in the test names, because apparently the benchmarks
;; are ran in alphabetic order

;; This is just to show the difference between a randomly generated network
;; and a real-world network. The randomly generated network does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark A-dijkstra-random-graph
  (let [network (fake-area/graph 1000)
        src     (rand-nth (alg/nodes network))
        dst     (rand-nth (alg/nodes network))
        router  (kt/->PedestrianRouter network)]
    (newline) (newline)
    (println "DIJKSTRA forward with:" (count (alg/nodes network)) "nodes")
    (c/quick-bench
      (let [coll (alg/dijkstra router #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

;; note src nil search will search for points greater or equal to src
;; I think nil src then search points less than src
(test/deftest ^:benchmark B-nearest-neighbour-search
  (let [network (deref (deref road/network))
        src     [8.645333, 50.087314]
        point   (:node/location (first (fastq/nearest-nodes network src)))]
    (newline) (newline)
    (println "Road network: nearest neighbour search with random src/dst")
    (println "B+ tree with:" (count (data/datoms network :eavt)) "nodes")
    (println "accuracy: " (geometry/haversine src point) "meters")
    (c/quick-bench (:node/location (first (fastq/nearest-nodes network src)))
                   :os :runtime :verbose)))

;;(type @kt/network) ;; force read
(test/deftest ^:benchmark C-pedestrian-road-network
  (let [network (deref (deref road/network))
        src     (first (fastq/nearest-nodes network [8.645333, 50.087314]))
        dst     (first (fastq/nearest-nodes network [8.635897, 50.104172]))
        router  (kt/->PedestrianRouter network)
        coll    (alg/dijkstra router #{src})]
    (newline) (newline)
    (println "Pedestrian routing with:" (count (alg/nodes network)) "nodes")
    (c/quick-bench (alg/shortest-path dst coll)
      :os :runtime :verbose)))

(test/deftest ^:benchmark D-transit-road-network
  (let [network    (deref (deref road/network))
        departure  (ZonedDateTime/parse "2018-05-07T10:15:30+02:00")
        stop-times (fastq/day-stop-times network (. departure (toLocalDate)))
        start      (Duration/between (LocalTime/MIDNIGHT) (. departure (toLocalTime)))
        src        (first (fastq/nearest-nodes network [8.645333, 50.087314]))
        dst        (first (fastq/nearest-nodes network [8.635897, 50.104172]))
        router     (transit/->StopTimesRouter network stop-times)
        coll       (alg/dijkstra router
                                 #{[src (. start (getSeconds))]})]
    (newline) (newline)
    (println "Transit routing with:" (count (alg/nodes network)) "nodes")
    (c/quick-bench (alg/shortest-path dst coll)
                   :os :runtime :verbose)))
