(ns hiposfer.kamal.graph.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.graph.generators :as g]
            [hiposfer.kamal.graph.algorithms :as alg]
            [hiposfer.kamal.osm :as osm]
            [hiposfer.kamal.directions :as direction]
            [hiposfer.kamal.graph.protocols :as rp]
            [hiposfer.kamal.libs.math :as math]
            [clojure.data.avl :as avl]))

;; This is just to show the difference between a randomly generated graph
;; and a real-world graph. The randomly generated graph does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph        (gen/generate (g/graph 1000))
        src          (key (first graph))
        dst          (key (last graph))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (reduce + (map (comp count rp/successors) (vals graph))) "edges")
    (println "**random graph")
    (c/quick-bench
      (let [coll (alg/dijkstra graph
                   :start-from #{src}
                   :value-by (partial direction/duration graph))]
        (reduce (fn [_ v] (when (= dst (key (first v)))
                            (reduced v)))
                nil
                coll))
      :os :runtime :verbose)))

(def networker (delay (osm/osm->network "resources/osm/saarland.osm.bz2")))
;(take 10 (:graph @networker)) ;; force read

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [graph        (:graph @networker) ;; force read
        src          (key (first graph))
        dst          (key (last graph))
        Cgraph       (alg/biggest-component (:graph @networker))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (reduce + (map (comp count rp/successors) (vals graph))) "edges")
    (println "saarland graph:")
    (c/quick-bench
      (let [coll (alg/dijkstra graph
                   :start-from #{src}
                   :value-by (partial direction/duration graph))]
        (reduce (fn [_ v] (when (= dst (key (first v)))
                            (reduced v)))
                nil
                coll))
      :os :runtime :verbose)
    (println "--------")
    (println "using only strongly connected components of the original graph")
    (c/quick-bench
      (let [coll (alg/dijkstra Cgraph
                   :start-from #{src}
                   :value-by (partial direction/duration Cgraph))]
        (reduce (fn [_ v] (when (= dst (key (first v)))
                            (reduced v)))
                nil
                coll))
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