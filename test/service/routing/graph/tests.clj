(ns service.routing.graph.tests
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            ;[clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is deftest]]
            [service.routing.graph.algorithms :as alg]
            [service.routing.graph.protocols  :as rp]
            [service.routing.graph.generators :as g]
            [service.routing.directions :as direction]))

;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
(def rosetta {1 {:out-arcs [{:dst 2 :length 7  :kind :other}
                            {:dst 3 :length 9  :kind :other}
                            {:dst 6 :length 14 :kind :other}]
                 :in-arcs  nil}
              2 {:out-arcs [{:dst 3 :length 10 :kind :other}
                            {:dst 4 :length 15 :kind :other}]
                 :in-arcs  [{:src 1 :length 7  :kind :other}]}
              3 {:out-arcs [{:dst 4 :length 11 :kind :other}

                            {:dst 6 :length 2  :kind :other}]
                 :in-arcs  [{:src 1 :length 9  :kind :other}
                            {:src 2 :length 10 :kind :other}]}
              4 {:out-arcs [{:dst 5 :length 6  :kind :other}]
                 :in-arcs  [{:src 2 :length 15 :kind :other}
                            {:src 3 :length 11 :kind :other}]}
              5 {:out-arcs [{:dst 6 :length 9  :kind :other}]
                 :in-arcs  [{:src 4 :length 6  :kind :other}]}
              6 {:out-arcs nil
                 :in-arcs  [{:src 1 :length 14 :kind :other}
                            {:src 3 :length 2  :kind :other}
                            {:src 5 :length 9  :kind :other}]}})

;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

(deftest shortest-path
  (let [dst       5
        performer (alg/dijkstra rosetta
                    :value-by service.routing.directions/length
                    :start-from #{1})
        traversal (reduce (fn [res v] (when (= dst (key v)) v))
                      nil
                      performer)]
    (is (not (nil? traversal)) "shortest path not found")
    (is (= '(5 4 3 1) (map key (rp/path traversal))) "shortest path doesnt traverse expected nodes")))

(deftest all-paths
  (let [performer (alg/dijkstra rosetta
                                :value-by service.routing.directions/length
                                :start-from #{1})
        traversal (into {} (map (juxt key (comp rp/cost val)))
                          performer)]
    (is (not (nil? traversal)) "shortest path not found")
    (is (= {1 0.0, 2 7.0, 3 9.0, 4 20.0, 5 26.0, 6 11.0} traversal) "shortest path doesnt traverse expected nodes")))

;(clojure.test/run-tests)

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
(defspec deterministic
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (g/graph 10) 1000)]
    (let [src  (rand-nth (keys graph))
          dst  (rand-nth (keys graph))
          coll (alg/dijkstra graph :start-from #{src}
                             :direction ::alg/forward
                             :value-by direction/length)]
      (apply = (repeatedly 10 #(reduce (fn [_ v] (when (= dst (key v)) (reduced (key v))))
                                       nil
                                       coll))))))

;(tc/quick-check 100 deterministic)
