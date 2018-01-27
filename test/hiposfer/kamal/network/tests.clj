(ns hiposfer.kamal.network.tests
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is deftest]]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.services.routing.directions :as direction]
            [hiposfer.kamal.network.generators :as ng]
            [hiposfer.kamal.network.graph.core :as graph]))

;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
(def rosetta {1 {:outgoing {2 {:src 1 :dst 2 :length 7}
                            3 {:src 1 :dst 3 :length 9}
                            6 {:src 1 :dst 6 :length 14}}}
              2 {:outgoing {3 {:src 2 :dst 3 :length 10}
                            4 {:src 2 :dst 4 :length 15}}
                 :incoming {1 {:src 1 :dst 2 :length 7}}}
              3 {:outgoing {4 {:src 3 :dst 4 :length 11}
                            6 {:src 3 :dst 6 :length 2}}
                 :incoming {1 {:src 1 :dst 3 :length 9}
                            2 {:src 2 :dst 3 :length 10}}}
              4 {:outgoing {5 {:src 4 :dst 5 :length 6}}
                 :incoming {2 {:src 2 :dst 4 :length 15}
                            3 {:src 3 :dst 4 :length 11}}}
              5 {:outgoing {6 {:src 5 :dst 6 :length 9}}
                 :incoming {4 {:src 4 :dst 5 :length 6}}}
              6 {:incoming {1 {:src 1 :dst 6 :length 14}
                            3 {:src 3 :dst 6 :length 2}
                            5 {:src 5 :dst 6 :length 9}}}})

;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

(deftest shortest-path
  (let [dst       5
        performer (alg/dijkstra rosetta
                                (fn [arc _] (:length arc))
                                #{1})
        traversal (alg/shortest-path dst performer)]
    (is (not (empty? traversal))
        "shortest path not found")
    (is (= '(5 4 3 1) (map key traversal))
        "shortest path doesnt traverse expected nodes")))

(deftest all-paths
  (let [performer (alg/dijkstra rosetta
                                (fn length [arc _] (:length arc))
                                #{1})
        traversal (into {} (comp (map first)
                                 (map (juxt key (comp np/cost val))))
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
  (prop/for-all [graph (gen/such-that not-empty (ng/graph 10) 1000)]
    (let [src  (rand-nth (keys graph))
          dst  (rand-nth (keys graph))
          coll (alg/dijkstra graph
                             (partial direction/duration graph)
                             #{src})
          results (for [_ (range 10)]
                    (alg/shortest-path dst coll))]
      (or (every? nil? results)
          (and (apply = (map (comp key first) results))
               (apply = (map (comp np/cost val first) results)))))))

; -------------------------------------------------------------------
; The Dijkstra algorithm cost is monotonic (increasing)
; https://en.wikipedia.org/wiki/Monotonic_function
(defspec monotonic
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (ng/graph 10) 1000)]
    (let [src    (rand-nth (keys graph))
          dst    (rand-nth (keys graph))
          coll   (alg/dijkstra graph
                               (partial direction/duration graph)
                               #{src})
          result (alg/shortest-path dst coll)]
      (is (or (nil? result)
              (apply >= (concat (map (comp np/cost val) result)
                                [0])))
          "returned path is not monotonic"))))

; -------------------------------------------------------------------
; If the distance of two nodes is 0 and no edge has a 0 cost,
; then the two nodes MUST be the same
; Ddf(P,Q) = 0 if P = Q
(defspec symmetry
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (ng/graph 10) 1000)]
    (let [src  (rand-nth (keys graph))
          coll (alg/dijkstra graph
                             (partial direction/duration graph)
                             #{src})
          result (alg/shortest-path src coll)]
      (is (not-empty result)
          "no path from node to himself found")
      (is (= 1 (count result))
          "more than one node has to be traverse to reach itself"))))

; -------------------------------------------------------------------
; The biggest strongly connected component of a network must be at most
; as big as the original network
(defspec components
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (ng/graph 10) 1000)]
    (let [graph2 (alg/biggest-component graph)]
      (is (graph/graph? graph2)
          "biggest component is not a graph")
      (is (<= (count graph2) (count graph))
          "biggest component is bigger than original graph"))))


; -------------------------------------------------------------------
; The removal of a node from a Graph should also eliminate all of its
; links. NOTE: The dijkstra below only stops after exploring the complete
; network. So if there is any open link, it will throw an exception.
; this use to throw an exception so we leave it here for testing purposes :)
(defspec routable-components
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (ng/graph 10) 1000)]
    (let [graph2 (alg/biggest-component graph)
          src    (key (first graph2))
          coll   (alg/dijkstra graph2
                               (partial direction/duration graph2)
                               #{src})]
      (is (seq? (reduce (fn [r v] v) nil coll))
          "biggest components should not contain links to nowhere"))))

;(clojure.test/run-tests)
