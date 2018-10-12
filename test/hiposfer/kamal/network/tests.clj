(ns hiposfer.kamal.network.tests
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test :refer [is deftest]]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [datascript.core :as data]
            [hiposfer.kamal.services.routing.core :as router]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.specs.directions :as dataspecs]
            [hiposfer.kamal.services.routing.transit :as transit]
            [hiposfer.kamal.services.routing.directions :as dir]
            [expound.alpha :as expound]))

;; Example taken from
;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
;; we assume bidirectional links
(def rosetta [{:node/id 1}
              {:node/id 2}
              {:node/id 3}
              {:node/id 4}
              {:node/id 5}
              {:node/id 6}
              {:node/id 1
               :node/successors #{[:node/id 2] [:node/id 3] [:node/id 6]}}
              {:node/id 2
               :node/successors #{[:node/id 3] [:node/id 4]}}
              {:node/id 3
               :node/successors #{[:node/id 4] [:node/id 6]}}
              {:node/id 4
               :node/successors #{[:node/id 5]}}
              {:node/id 5
               :node/successors #{[:node/id 6]}}
              {:node/id 6
               :node/successors #{}}])

;; HACK: It shouldnt be like this but I am not going to modify the schema just
;; to make it pretty
(def lengths {1 {2 7, 3 9, 6 14}
              2 {3 10, 4 15}
              3 {4 11, 6 2}
              4 {5 6}
              5 {6 9}})

;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

(defrecord RosettaRouter [graph]
  np/Router
  (relax [this dst trail]
    (let [src-id (:node/id (key (first trail)))
          dst-id (:node/id dst)]
      (get-in lengths [src-id dst-id])))
  (successors [this node]
    (:node/successors node)))

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
    (is (= '(5 4 3 1) (map (comp :node/id key) traversal))
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
    (is (= {1 0, 2 7, 3 9, 4 20, 5 26, 6 11}
           traversal)
        "shortest path doesnt traverse expected nodes")))

(defrecord PedestrianRouter [graph]
  np/Router
  (relax [this dst trail]
    (transit/duration graph dst trail))
  (successors [this node]
    (fastq/node-successors graph node)))

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
(defspec deterministic
  100; tries
  (prop/for-all [i (gen/large-integer* {:min 10 :max 20})]
    (let [graph   @(router/pedestrian-graph {:SIZE i})
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
  (prop/for-all [i (gen/large-integer* {:min 10 :max 20})]
     (let [graph   @(router/pedestrian-graph {:SIZE i})
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
  (prop/for-all [i (gen/large-integer* {:min 10 :max 20})]
    (let [graph   @(router/pedestrian-graph {:SIZE i})
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
  (prop/for-all [i (gen/large-integer* {:min 10 :max 20})]
    (let [graph  @(router/pedestrian-graph {:SIZE i})
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
  (prop/for-all [i (gen/large-integer* {:min 10 :max 20})]
    (let [graph   @(router/pedestrian-graph {:SIZE i})
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
  (prop/for-all [i (gen/large-integer* {:min 10 :max 20})]
    (let [graph   @(router/pedestrian-graph {:SIZE i})
          request  (gen/generate (s/gen ::dataspecs/params))
          result   (dir/direction graph request)]
      (is (s/valid? ::dataspecs/directions result)
          (str (expound/expound-str ::dataspecs/directions result))))))

;(clojure.test/run-tests)
