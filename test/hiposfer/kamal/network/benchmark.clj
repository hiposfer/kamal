(ns hiposfer.kamal.network.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.network.generators :as ng]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.services.routing.directions :as directions]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.network.graph.core :as graph]
            [hiposfer.kamal.services.routing.core :as router]
            [datascript.core :as data]))

;; This is just to show the difference between a randomly generated network
;; and a real-world network. The randomly generated network does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph  (gen/generate (ng/graph 1000))
        ;conn   (data/create-conn router/schema) TODO
        ;(take-last 5 (data/datoms @conn :aevt :node/id))
        src    (key (first graph))
        dst    (key (last graph))]
    (println "\n\nDIJKSTRA forward with:" (count graph) "nodes and"
             (count (graph/edges graph)) "edges")
    (println "**random graph")
    (c/quick-bench
      (let [coll (alg/dijkstra graph (partial directions/duration graph) #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

(def networker (delay
                 (time
                   (let [data (osm/datomize "resources/osm/saarland.min.osm.bz2")
                         conn (data/create-conn router/schema)]
                     (data/transact! conn data)
                     @conn)))) ;; dont allow transact anymore


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
  (let [src   [7.038535 49.345088]
        point (:v (first (data/index-range @networker :node/location src nil)))]
    (println "\n\nsaarland graph: nearest neighbour search with random src/dst")
    (println "B+ tree with:" (count (data/datoms @networker :eavt)) "nodes")
    (println "accuraccy: " (geometry/haversine src point))
    (c/quick-bench (first (data/index-range @networker :node/location src nil))
                   :os :runtime :verbose)
    (println "--------")
    (println "BRUTE force with:" (count (data/datoms @networker :eavt)) "nodes")
    (println "accuraccy: " (geometry/haversine src (second (brute-nearest @networker src))))
    (c/quick-bench (brute-nearest @networker src)
                   :os :runtime :verbose)))
