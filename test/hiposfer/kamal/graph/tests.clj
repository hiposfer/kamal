(ns hiposfer.kamal.graph.tests
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is deftest]]
            [hiposfer.kamal.graph.algorithms :as alg]
            [hiposfer.kamal.graph.protocols :as rp]
            [hiposfer.kamal.graph.generators :as g]
            [hiposfer.kamal.directions :as direction]
            [clojure.set :as set]
            [hiposfer.kamal.graph.core :as route]
            [clojure.test.check :as tc]))

;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
(def rosetta {1 {:outgoing {2 {:dst 2 :length 7}
                            3 {:dst 3 :length 9}
                            6 {:dst 6 :length 14}}}
              2 {:outgoing {3 {:dst 3 :length 10}
                            4 {:dst 4 :length 15}}
                 :incoming {1 {:src 1 :length 7}}}
              3 {:outgoing {4 {:dst 4 :length 11}
                            6 {:dst 6 :length 2}}
                 :incoming {1 {:src 1 :length 9}
                            2 {:src 2 :length 10}}}
              4 {:outgoing {5 {:dst 5 :length 6}}
                 :incoming {2 {:src 2 :length 15}
                            3 {:src 3 :length 11}}}
              5 {:outgoing {6 {:dst 6 :length 9}}
                 :incoming {4 {:src 4 :length 6}}}
              6 {:incoming {1 {:src 1 :length 14}
                            3 {:src 3 :length 2}
                            5 {:src 5 :length 9}}}})

;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

(deftest shortest-path
  (let [dst       5
        performer (alg/dijkstra rosetta
                    :value-by (fn [arc _] (:length arc))
                    :start-from #{1})
        traversal (reduce (fn [_ v] (when (= dst (key (first v)))
                                      (reduced v)))
                      nil
                      performer)]
    (is (not (empty? traversal))
        "shortest path not found")
    (is (= '(5 4 3 1) (map key traversal))
        "shortest path doesnt traverse expected nodes")))

(deftest all-paths
  (let [performer (alg/dijkstra rosetta
                                :value-by (fn length [arc _] (:length arc))
                                :start-from #{1})
        traversal (into {} (comp (map first)
                                 (map (juxt key (comp rp/cost val))))
                           performer)]
    (is (not (nil? traversal))
        "shortest path not found")
    (is (= {1 0, 2 7, 3 9, 4 20, 5 26, 6 11}
           traversal)
        "shortest path doesnt traverse expected nodes")))

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
                                   :value-by (partial direction/duration graph))
          results (for [_ (range 10)]
                    (reduce (fn [_ v] (when (= dst (key (first v)))
                                        (reduced v)))
                            nil coll))]
      (or (every? nil? results)
          (and (apply = (map (comp key first) results))
               (apply = (map (comp rp/cost val first) results)))))))

; -------------------------------------------------------------------
; The Dijkstra algorithm cost is monotonic (increasing)
; https://en.wikipedia.org/wiki/Monotonic_function
(defspec monotonic
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (g/graph 10) 1000)]
    (let [src  (rand-nth (keys graph))
          dst  (rand-nth (keys graph))
          coll (alg/dijkstra graph
                             :start-from #{src}
                             :value-by (partial direction/duration graph))
          result (reduce (fn [_ v] (when (= dst (key (first v)))
                                     (reduced v)))
                         nil
                         coll)]
      (or (nil? result)
          (apply >= (concat (map (comp rp/cost val)
                                 result)
                            [0]))))))

; -------------------------------------------------------------------
; If the distance of two nodes is 0 and no edge has a 0 cost,
; then the two nodes MUST be the same
; Ddf(P,Q) = 0 if P = Q
(defspec symmetry
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (g/graph 10) 1000)]
    (let [src  (rand-nth (keys graph))
          coll (alg/dijkstra graph
                             :start-from #{src}
                             :value-by (partial direction/duration graph))
          result (reduce (fn [_ v] (when (= src (key (first v)))
                                     (reduced v)))
                         nil
                         coll)]
      (and (not (empty? result))
           (= 1 (count result))))))

; -------------------------------------------------------------------
; The biggest strongly connected component of a graph must be at most
; as big as the original graph
(defspec components
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (g/graph 10) 1000)]
    (let [graph2 (alg/biggest-component graph)]
      (<= (count graph2) (count graph)))))

; -------------------------------------------------------------------
; The removal of a node from a Graph should also eliminate all of its
; links
(defspec detachable
  300; tries
  (prop/for-all [graph (gen/such-that not-empty (g/graph 1) 1000)]
    (let [graph2 (route/detach graph (ffirst graph))
          ids    (into #{} (mapcat (juxt rp/src rp/dst)
                                   (route/edges graph)))
          ids2   (into #{} (mapcat (juxt rp/src rp/dst)
                                   (route/edges graph2)))]
      (not-empty (set/difference ids ids2)))))

;(clojure.test/run-tests)

(def detachable2
  (prop/for-all [graph (gen/such-that not-empty (g/graph 1) 1000)]
    (let [graph2 (route/detach graph (ffirst graph))
          ids    (into #{} (mapcat (juxt rp/src rp/dst)
                                   (route/edges graph)))
          ids2   (into #{} (mapcat (juxt rp/src rp/dst)
                                   (route/edges graph2)))]
      (not-empty (set/difference ids ids2)))))

(tc/quick-check 500 detachable2)

(def g
  {0 #hiposfer.kamal.graph.core.NodeInfo{:lon -0.006399357691407204,
                                         :lat 5.125,
                                         :outgoing nil,
                                         :incoming nil},
   1 #hiposfer.kamal.graph.core.NodeInfo{:lon 1.200036633759737,
                                         :lat -31.0,
                                         :outgoing {2 #hiposfer.kamal.graph.core.Edge{:src 1,
                                                                                      :dst 2,
                                                                                      :way 3},
                                                    1 #hiposfer.kamal.graph.core.Edge{:src 1,
                                                                                      :dst 1,
                                                                                      :way 11}},
                                         :incoming {3 #hiposfer.kamal.graph.core.Edge{:src 3,
                                                                                      :dst 1,
                                                                                      :way 11},
                                                    2 #hiposfer.kamal.graph.core.Edge{:src 2,
                                                                                      :dst 1,
                                                                                      :way 0},
                                                    1 #hiposfer.kamal.graph.core.Edge{:src 1,
                                                                                      :dst 1,
                                                                                      :way 11}}},
   2 #hiposfer.kamal.graph.core.NodeInfo{:lon -11.0,
                                         :lat -0.5859527587890625,
                                         :outgoing {1 #hiposfer.kamal.graph.core.Edge{:src 2,
                                                                                      :dst 1,
                                                                                      :way 0},
                                                    3 #hiposfer.kamal.graph.core.Edge{:src 2,
                                                                                      :dst 3,
                                                                                      :way 11},
                                                    2 #hiposfer.kamal.graph.core.Edge{:src 2,
                                                                                      :dst 2,
                                                                                      :way 3}},
                                         :incoming {3 #hiposfer.kamal.graph.core.Edge{:src 3,
                                                                                      :dst 2,
                                                                                      :way 5},
                                                    1 #hiposfer.kamal.graph.core.Edge{:src 1,
                                                                                      :dst 2,
                                                                                      :way 3},
                                                    2 #hiposfer.kamal.graph.core.Edge{:src 2,
                                                                                      :dst 2,
                                                                                      :way 3}}},
   3 #hiposfer.kamal.graph.core.NodeInfo{:lon -0.1044921875,
                                         :lat -0.051645700936205685,
                                         :outgoing {1 #hiposfer.kamal.graph.core.Edge{:src 3,
                                                                                      :dst 1,
                                                                                      :way 11},
                                                    2 #hiposfer.kamal.graph.core.Edge{:src 3,
                                                                                      :dst 2,
                                                                                      :way 5},
                                                    3 #hiposfer.kamal.graph.core.Edge{:src 3,
                                                                                      :dst 3,
                                                                                      :way 11}},
                                         :incoming {3 #hiposfer.kamal.graph.core.Edge{:src 3,
                                                                                      :dst 3,
                                                                                      :way 11},
                                                    2 #hiposfer.kamal.graph.core.Edge{:src 2,
                                                                                      :dst 3,
                                                                                      :way 11}}}})

#_(let [graph2 (route/detach g (ffirst g))]
      ids    (into #{} (mapcat (juxt rp/src rp/dst)
                               (route/edges g)))
      ids2   (into #{} (mapcat (juxt rp/src rp/dst)
                               (route/edges graph2)))
    (not-empty (set/difference ids ids2)))
