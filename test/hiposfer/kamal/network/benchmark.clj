(ns hiposfer.kamal.network.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.network.generators :as ng]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.services.routing.directions :as directions]
            [hiposfer.kamal.libs.geometry :as geometry]
            [clojure.data.avl :as avl]
            [hiposfer.kamal.network.graph.core :as graph]))

;; This is just to show the difference between a randomly generated network
;; and a real-world network. The randomly generated network does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph        (gen/generate (ng/graph 1000))
        src          (key (first graph))
        dst          (key (last graph))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (count (graph/edges graph)) "edges")
    (println "**random graph")
    (c/quick-bench
      (let [coll (alg/dijkstra graph (partial directions/duration graph) #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

(def networker (-> (osm/network "resources/osm/saarland.min.osm.bz2")
                   (osm/complete)
                   (delay)))

;(take 10 (:network @networker)) ;; force read
;(take 10 (alg/biggest-component (:network @networker)))

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [graph        (:graph @networker) ;; force read
        Cgraph       (alg/biggest-component (:graph @networker))
        ;; the src and dst might have been removed from the original network
        src          (key (first Cgraph))
        dst          (key (last Cgraph))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (count (graph/edges graph)) "edges")
    (println "saarland graph:")
    (c/quick-bench
      (let [coll (alg/dijkstra graph (partial directions/duration graph) #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)
    (println "--------")
    (println "using only strongly connected components of the original graph")
    (println "with:" (count Cgraph) "nodes and"
             (count (graph/edges Cgraph)) "edges")
    (c/quick-bench
      (let [coll (alg/dijkstra Cgraph (partial directions/duration Cgraph) #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

(defn- brute-nearest
  "search the nearest node in network to point using the distance function f.
  f defaults to the euclidean distance squared"
  ([network point f]
   (reduce (fn [best entry] (if (< (f point (second entry))
                                   (f point (second best)))
                              entry
                              best))
           (first (:graph network))
           (:graph network)))
  ([network point]
   (brute-nearest network point geometry/euclidean-pow2)))

;; note >= search will approximate any point with lat, lon less than point
;; to the minimum point in neighbours. <= does the same but approximates to
;; the maximum.
(test/deftest ^:benchmark nearest-neighbour-search
  (let [neighbours (:neighbours @networker)
        graph      (:graph @networker)
        src        [7.038535 49.345088]]
    (println "\n\nsaarland graph: nearest neighbour search with random src/dst")
    (println "AVL tree with:" (count graph) "nodes")
    ;; https://github.com/clojure/data.avl
    ;; They add some memory overhead -- a reference and two ints per key. The
    ;; additional node fields are used to support transients (one reference
    ;; field per key), rank queries (one int) and the rebalancing algorithm
    ;; itself (the final int).
    ;; + 1 due to integer key duplicated
    (println "accuraccy: " (geometry/haversine src (first (avl/nearest neighbours >= src))))
    (c/quick-bench (avl/nearest neighbours >= src)
                   :os :runtime :verbose)
    (println "--------")
    (println "BRUTE force with:" (count graph) "nodes")
    (println "accuraccy: " (geometry/haversine src (second (brute-nearest @networker src))))
    (c/quick-bench (brute-nearest @networker src)
                   :os :runtime :verbose)))
