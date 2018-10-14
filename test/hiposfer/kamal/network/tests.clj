(ns hiposfer.kamal.network.tests
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.services.routing.core :as router]
            [hiposfer.kamal.network.generators :as fake-area]
            [hiposfer.kamal.specs.directions :as dataspecs]
            [hiposfer.kamal.services.routing.transit :as transit]
            [hiposfer.kamal.services.routing.directions :as dir]
            [expound.alpha :as expound]
            [datascript.core :as data]))

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

(defrecord RosettaRouter [graph]
  np/Router
  (relax [this arc trail]
    (+ (val (first trail))
       (:arc/length arc))))

(deftest shortest-path
  (let [network   (data/create-conn router/schema)
        _         (data/transact! network rosetta)
        dst       (data/entity @network [:node/id 5])
        src       (data/entity @network [:node/id 1])
        router    (->RosettaRouter @network)
        performer (alg/dijkstra router #{src})
        traversal (alg/shortest-path dst performer)]
    (is (not (empty? traversal))
        "shortest path not found")
    (is (= '(5 6 3 1) (map (comp :node/id key) traversal))
        "shortest path doesnt traverse expected nodes")))

(deftest all-paths
  (let [network   (data/create-conn router/schema)
        _         (data/transact! network rosetta)
        src       (data/entity @network [:node/id 1])
        router    (->RosettaRouter @network)
        performer (alg/dijkstra router #{src})
        traversal (into {} (comp (map first)
                                 (map (juxt (comp :node/id key) (comp np/cost val))))
                           performer)]
    (is (not (nil? traversal))
        "shortest path not found")
    (is (= {1 0, 2 7, 3 9, 4 20, 5 20, 6 11}
           traversal)
        "shortest path doesnt traverse expected nodes")))

(defrecord PedestrianRouter [graph]
  np/Router
  (relax [this arc trail]
    (let [[src value] (first trail)
          dst         (np/dst arc)]
      (when (transit/node? (np/dst arc))
        (+ value (transit/walk-time (:node/location src)
                                    (:node/location dst)))))))

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
(defspec deterministic
  100; tries
  (prop/for-all [size (gen/large-integer* {:min 10 :max 20})]
    (let [graph    (fake-area/graph size)
          src      (rand-nth (alg/nodes graph))
          dst      (rand-nth (alg/nodes graph))
          router   (->PedestrianRouter graph)
          coll     (alg/dijkstra router #{src})
          results  (for [_ (range 10)]
                     (alg/shortest-path dst coll))]
      (is (or (every? nil? results)
              (and (apply = (map (comp key first) results))
                   (apply = (map (comp np/cost val first) results))))
          "not deterministic behavior"))))

; -------------------------------------------------------------------
; The Dijkstra algorithm cost is monotonic (increasing)
; https://en.wikipedia.org/wiki/Monotonic_function
(defspec monotonic
  100; tries
  (prop/for-all [size (gen/large-integer* {:min 10 :max 20})]
     (let [graph    (fake-area/graph size)
           src      (rand-nth (alg/nodes graph))
           dst      (rand-nth (alg/nodes graph))
           router   (->PedestrianRouter graph)
           coll     (alg/dijkstra router #{src})
           result   (alg/shortest-path dst coll)]
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
  (prop/for-all [size (gen/large-integer* {:min 10 :max 20})]
    (let [graph    (fake-area/graph size)
          src      (rand-nth (alg/nodes graph))
          router   (->PedestrianRouter graph)
          coll     (alg/dijkstra router #{src})
          result   (alg/shortest-path src coll)]
      (is (not-empty result)
          "no path from node to himself found")
      (is (= 1 (count result))
          "more than one node has to be traverse to reach itself"))))

; -------------------------------------------------------------------
; The biggest strongly connected component of a network must be at most
; as big as the original network
(defspec components
  100; tries
  (prop/for-all [size (gen/large-integer* {:min 10 :max 20})]
    (let [graph   (fake-area/graph size)
          router  (->PedestrianRouter graph)
          r1      (alg/looners graph router)
          graph2  (data/db-with graph (for [i r1 ] [:db.fn/retractEntity (:db/id i)]))
          router2 (->PedestrianRouter graph2)
          r2      (alg/looners graph2 router2)]
      (is (empty? r2)
          "looners should be empty for a strongly connected graph"))))


; -------------------------------------------------------------------
; The removal of a node from a Graph should also eliminate all of its
; links. NOTE: The dijkstra below only stops after exploring the complete
; network. So if there is any open link, it will throw an exception.
; this use to throw an exception so we leave it here for testing purposes :)
(defspec routable-components
  100; tries
  (prop/for-all [size (gen/large-integer* {:min 10 :max 20})]
    (let [graph   (fake-area/graph size)
          router  (->PedestrianRouter graph)
          r1      (alg/looners graph router)
          graph2  (data/db-with graph (for [i r1 ] [:db.fn/retractEntity (:db/id i)]))
          src     (rand-nth (alg/nodes graph2))
          router  (->PedestrianRouter graph2)
          coll    (alg/dijkstra router #{src})]
      (is (seq? (reduce (fn [r v] v) nil coll))
          "biggest components should not contain links to nowhere"))))

; -----------------------------------------------------------------
; generative tests for the direction endpoint
(defspec generative-directions
  100; tries
  (prop/for-all [size (gen/large-integer* {:min 10 :max 20})]
    (let [graph    (fake-area/graph size)
          request  (gen/generate (s/gen ::dataspecs/params))
          result   (dir/direction graph request)]
      (is (s/valid? ::dataspecs/directions result)
          (str (expound/expound-str ::dataspecs/directions result))))))

;(clojure.test/run-tests)
