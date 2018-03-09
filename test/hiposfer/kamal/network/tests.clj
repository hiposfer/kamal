(ns hiposfer.kamal.network.tests
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is deftest]]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.services.routing.directions :as direction]
            [hiposfer.kamal.network.generators :as ng]
            [hiposfer.kamal.network.graph.core :as graph]
            [hiposfer.kamal.libs.tool :as tool]
            [datascript.core :as data]
            [hiposfer.kamal.services.routing.core :as router]))

;; Example taken from
;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
;; we assume bidirectional links
(def rosetta [{:node/id 1
               :node/successors #{[:node/id 2] [:node/id 3] [:node/id 6]}}
              {:node/id 2
               :node/successors #{[:node/id 3] [:node/id 4]}}
              {:node/id 3
               :node/successors #{[:node/id 4] [:node/id 6]}}
              {:node/id 4
               :node/successors #{[:node/id 5]}}
              {:node/id 5
               :node/successors #{[:node/successors 6]}}
              {:node/id 6
               :node/successors #{}}])

;; hack. It shouldnt be like this but I am not going to modify the schema just
;; to make it pretty
(def lengths {1 {2 7, 3 9, 6 14}
              2 {3 10, 4 15}
              3 {4 11, 6 2}
              4 {5 6}
              5 {6 9}})

;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

(defn- length
  [network dst trail]
  (let [src    (data/entity network (key (first trail)))
        dst    (data/entity network dst)]
    (get-in lengths [src dst])))

(deftest shortest-path
  (let [dst       5
        performer (alg/dijkstra rosetta
                                #(length rosetta %1 %2)
                                tool/node-successors
                                #{1})
        traversal (alg/shortest-path dst performer)]
    (is (not (empty? traversal))
        "shortest path not found")
    (is (= '(5 4 3 1) (map key traversal))
        "shortest path doesnt traverse expected nodes")))

(deftest all-paths
  (let [performer (alg/dijkstra rosetta
                                #(length rosetta %1 %2)
                                tool/node-successors
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
    (let [network (data/create-conn router/schema)
          _ (data/transact! network graph)
          src  (rand-nth (alg/node-ids @network))
          dst  (rand-nth (alg/node-ids @network))
          coll (alg/dijkstra @network
                             #(direction/duration @network %1 %2)
                             tool/node-successors
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
    (let [network (data/create-conn router/schema)
          _ (data/transact! network graph)
          src  (rand-nth (alg/node-ids @network))
          dst  (rand-nth (alg/node-ids @network))
          coll (alg/dijkstra @network
                             #(direction/duration @network %1 %2)
                             tool/node-successors
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
    (let [network (data/create-conn router/schema)
          _ (data/transact! network graph)
          src  (rand-nth (alg/node-ids @network))
          coll (alg/dijkstra @network
                             #(direction/duration @network %1 %2)
                             tool/node-successors
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
    (let [network (data/create-conn router/schema)
          _ (data/transact! network graph)
          r1      (alg/looners @network)
          _ (data/transact! network (map #(vector :db.fn/retractEntity %) r1))
          r2      (alg/looners @network)]
      (is (empty? r2)
          "looners should be empty for a strongly connected graph"))))


; -------------------------------------------------------------------
; The removal of a node from a Graph should also eliminate all of its
; links. NOTE: The dijkstra below only stops after exploring the complete
; network. So if there is any open link, it will throw an exception.
; this use to throw an exception so we leave it here for testing purposes :)
(defspec routable-components
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (ng/graph 10) 1000)]
    (let [network (data/create-conn router/schema)
          _ (data/transact! network graph)
          src  (rand-nth (alg/node-ids @network))
          coll (alg/dijkstra @network
                             #(direction/duration @network %1 %2)
                             tool/node-successors
                             #{src})]
      (is (seq? (reduce (fn [r v] v) nil coll))
          "biggest components should not contain links to nowhere"))))

;(clojure.test/run-tests)
