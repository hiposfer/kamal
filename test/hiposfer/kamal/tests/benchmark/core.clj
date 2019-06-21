(ns hiposfer.kamal.tests.benchmark.core
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [datascript.core :as data]
            [hiposfer.kamal.router.util.geometry :as geometry]
            [hiposfer.kamal.router.util.fastq :as fastq]
            [hiposfer.kamal.router.generative :as kt]
            [hiposfer.kamal.router.road :as road]
            [hiposfer.kamal.router.transit :as transit]
            [hiposfer.kamal.router.generators :as fake-area]
            [hiposfer.kamal.router.graph :as graph]
            [hiposfer.kamal.router.algorithms.protocols :as np]
            [hiposfer.kamal.router.algorithms.dijkstra :as dijkstra])
  (:import (java.time ZonedDateTime Duration LocalTime)
           (hiposfer.kamal.router.graph PedestrianNode)))

;; NOTE: we put a letter in the test names, because apparently the benchmarks
;; are ran in alphabetic order

;; This is just to show the difference between a randomly generated network
;; and a real-world network. The randomly generated network does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark A-dijkstra-random-graph
  (let [network (fake-area/osm-gen 1000)
        src     (rand-nth (kt/nodes network))
        dst     (rand-nth (kt/nodes network))
        router  (kt/->PedestrianDatascriptRouter network)]
    (newline) (newline)
    (println "DIJKSTRA forward with:" (count (kt/nodes network)) "nodes")
    (c/quick-bench (dijkstra/shortest-path router #{(:db/id src)} (:db/id dst))
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

(defrecord PedestrianIntMapRouter [graph]
  np/Dijkstra
  (node [this id] (get graph id))
  (relax [this arc trail]
    (let [[src-id value] (first trail)
          dst-id         (np/dst arc)
          src            (get graph src-id)
          dst            (get graph dst-id)]
      (when (instance? PedestrianNode dst)
        (+ value (transit/walk-time (:location src)
                                    (:location dst)))))))

(test/deftest ^:benchmark C-pedestrian-road-network
  (let [network (deref (deref road/network))
        src     (first (fastq/nearest-nodes network [8.645333, 50.087314]))
        dst     (first (fastq/nearest-nodes network [8.635897, 50.104172]))
        router  (->PedestrianIntMapRouter (graph/create network))]
    (newline) (newline)
    (println "Pedestrian routing with:" (count (kt/nodes network)) "nodes")
    (c/quick-bench (dijkstra/shortest-path router #{(:db/id src)} (:db/id dst))
                   :os :runtime :verbose)))

(test/deftest ^:benchmark D-transit-road-network
  (let [network    (deref (deref road/network))
        departure  (ZonedDateTime/parse "2018-05-07T10:15:30+02:00")
        trips      (fastq/day-trips network (. departure (toLocalDate)))
        start      (Duration/between (LocalTime/MIDNIGHT) (. departure (toLocalTime)))
        src        (first (fastq/nearest-nodes network [8.645333, 50.087314]))
        dst        (first (fastq/nearest-nodes network [8.635897, 50.104172]))
        graph      (graph/create network)
        router     (transit/->TransitRouter network graph trips)
        start      [(:db/id src) (. start (getSeconds))]]
    (newline) (newline)
    (println "Transit routing with:" (count (kt/nodes network)) "nodes")
    (c/quick-bench (dijkstra/shortest-path router #{start} (:db/id dst))
                   :os :runtime :verbose)))

;(clojure.test/run-tests)
