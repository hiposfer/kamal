(ns hiposfer.kamal.router.unit
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hiposfer.kamal.router.algorithms.dijkstra :as dijkstra]
            [hiposfer.kamal.router.algorithms.protocols :as np]
            [hiposfer.kamal.router.core :as router]
            [datascript.core :as data])
  (:import (datascript.impl.entity Entity)))

;; Example taken from
;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
;; we assume bidirectional links
(def rosetta [{:node/id 1}
              {:node/id 2}
              {:node/id 3}
              {:node/id 4}
              {:node/id 5}
              {:node/id 6}
              {:edge/src [:node/id 1]
               :edge/dst [:node/id 2]
               :arc/length 7}
              {:edge/src [:node/id 1]
               :edge/dst [:node/id 3]
               :arc/length 9}
              {:edge/src [:node/id 1]
               :edge/dst [:node/id 6]
               :arc/length 14}
              {:edge/src [:node/id 2]
               :edge/dst [:node/id 3]
               :arc/length 10}
              {:edge/src [:node/id 2]
               :edge/dst [:node/id 4]
               :arc/length 15}
              {:edge/src [:node/id 3]
               :edge/dst [:node/id 4]
               :arc/length 11}
              {:edge/src [:node/id 3]
               :edge/dst [:node/id 6]
               :arc/length 2}
              {:edge/src [:node/id 4]
               :edge/dst [:node/id 5]
               :arc/length 6}
              {:edge/src [:node/id 5]
               :edge/dst [:node/id 6]
               :arc/length 9}])

;; --- directed arcs
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

;; --- bidirectional arcs ... see kotlin solution
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 20) (6 11))
;Shortest path: (1 3 6 5)

(extend-type Entity
  np/Arc
  (src [this] (:db/id (:edge/src this)))
  (dst [this] (:db/id (:edge/dst this)))
  np/Node
  (successors [this]
    (let [db (data/entity-db this)]
      (map #(data/entity db (:e %))
           (data/datoms db :avet :edge/src (:db/id this))))))

(defrecord RosettaRouter [network]
  np/Dijkstra
  (node [this k] (data/entity network k))
  (relax [this arc trail]
    (+ (val (first trail))
       (:arc/length arc))))

(deftest shortest-path
  (let [network   (-> (data/empty-db router/schema)
                      (data/with rosetta)
                      (:db-after))
        dst       (:db/id (data/entity network [:node/id 5]))
        src       (:db/id (data/entity network [:node/id 1]))
        router    (->RosettaRouter network)
        traversal (dijkstra/shortest-path router #{src} dst)]
    (is (not (empty? traversal))
        "shortest path not found")
    (is (= '(5 4 3 1) (map key traversal))
        "shortest path doesnt traverse expected nodes")))

(deftest all-paths
  (let [network   (-> (data/empty-db router/schema)
                      (data/with rosetta)
                      (:db-after))
        src       (:db/id (data/entity network [:node/id 1]))
        router    (->RosettaRouter network)
        view      (dijkstra/view router #{src})
        traversal (into {} (comp (map first)
                                 (map (juxt key (comp np/cost val))))
                        view)]
    (is (not (nil? traversal))
        "shortest path not found")
    (is (= {1 0, 2 7, 3 9, 4 20, 5 26, 6 11}
           traversal)
        "shortest path doesnt traverse expected nodes")))
