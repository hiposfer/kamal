(ns hiposfer.kamal.graph.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.graph.generators :as g]
            [hiposfer.kamal.graph.algorithms :as alg]
            [hiposfer.kamal.osm :as osm]
            [hiposfer.kamal.directions :as direction]
            [hiposfer.kamal.libs.math :as math]
            [clojure.data.avl :as avl]
            [hiposfer.kamal.graph.core :as graph]))

;; This is just to show the difference between a randomly generated graph
;; and a real-world graph. The randomly generated graph does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph        (gen/generate (g/graph 1000))
        src          (key (first graph))
        dst          (key (last graph))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (count (graph/edges graph)) "edges")
    (println "**random graph")
    (c/quick-bench
      (let [coll (alg/dijkstra (partial direction/duration graph) #{src} graph)]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

(def networker (delay (osm/osm->network "resources/osm/saarland.min.osm.bz2")))
;(take 10 (:graph @networker)) ;; force read
;(take 10 (alg/biggest-component (:graph @networker)))

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [graph        (:graph @networker) ;; force read
        Cgraph       (alg/biggest-component (:graph @networker))
        ;; the src and dst might have been removed from the original graph
        src          (key (first Cgraph))
        dst          (key (last Cgraph))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (count (graph/edges graph)) "edges")
    (println "saarland graph:")
    (c/quick-bench
      (let [coll (alg/dijkstra (partial direction/duration graph) #{src} graph)]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)
    (println "--------")
    (println "using only strongly connected components of the original graph")
    (println "with:" (count Cgraph) "nodes and"
             (count (graph/edges Cgraph)) "edges")
    (c/quick-bench
      (let [coll (alg/dijkstra (partial direction/duration Cgraph) #{src} Cgraph)]
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
   (brute-nearest network point math/euclidean-pow2)))

;; only the connected nodes
(test/deftest ^:benchmark nearest-neighbour-search
  (let [neighbours   (:neighbours @networker)
        graph        (:graph @networker)
        src          (val (last graph))]
    (println "\n\nsaarland graph: nearest neighbour search with random src/dst")
    (println "AVL tree with:" (count graph) "nodes")
    ;; https://github.com/clojure/data.avl
    ;; They add some memory overhead -- a reference and two ints per key. The
    ;; additional node fields are used to support transients (one reference
    ;; field per key), rank queries (one int) and the rebalancing algorithm
    ;; itself (the final int).
    ;; + 1 due to integer key duplicated
    (println "extra space required:" (/ (* (* 4 8) (count graph)) 1e6) " MB")
    (c/quick-bench (avl/nearest neighbours <= src)
      :os :runtime :verbose)
    (println "--------")
    (println "BRUTE force with:" (count graph) "nodes")
    (c/quick-bench (brute-nearest @networker src)
      :os :runtime :verbose)))

;(clojure.test/run-tests)