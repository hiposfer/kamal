(ns hiposfer.kamal.network.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.network.generators :as ng]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.parsers.osm :as osm]
            [hiposfer.kamal.libs.tool :as tool]
            [hiposfer.kamal.services.routing.directions :as directions]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.services.routing.core :as router]
            [datascript.core :as data]))

;; This is just to show the difference between a randomly generated network
;; and a real-world network. The randomly generated network does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [dt      (gen/generate (ng/graph 1000))
        network (data/create-conn router/schema)
        _       (data/transact! network dt)
        src     (rand-nth (alg/node-ids @network))
        dst     (rand-nth (alg/node-ids @network))]
    (println "\n\nDIJKSTRA forward with:" (count (alg/node-ids @network)) "nodes")
    (println "**random graph")
    (c/quick-bench
      (let [coll (alg/dijkstra @network #(directions/duration @network %1 %2)
                                         tool/node-successors
                                         #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

(def network (delay
                 (time
                   (let [data (osm/datomize "resources/osm/saarland.min.osm.bz2")
                         conn (data/create-conn router/schema)]
                     (data/transact! conn data)
                     conn))))

;(take 10 (:network @networker)) ;; force read
;(take 10 (alg/biggest-component (:network @networker)))

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [src     (first (alg/node-ids @@network))
        dst     (last (alg/node-ids @@network))
        removable (alg/looners @@network)]
    (println "\n\nDIJKSTRA forward with:" (count (alg/node-ids @@network)) "nodes")
    (println "saarland graph:")
    (c/quick-bench
      (let [coll (alg/dijkstra @@network #(directions/duration @@network %1 %2)
                                         tool/node-successors
                                         #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)
    (println "--------")
    (println "using only strongly connected components of the original graph")
    (data/transact! @network (map #(vector :db.fn/retractEntity %) removable))
    (println "with:" (count (alg/node-ids @@network)) "nodes")
    (c/quick-bench
      (let [coll (alg/dijkstra @@network #(directions/duration @@network %1 %2)
                               tool/node-successors
                               #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

;; note src nil search will search for points greater or equal to src
;; I think nil src then search points less than src
(test/deftest ^:benchmark nearest-neighbour-search
  (let [src   [7.038535 49.345088]
        point (:v (tool/nearest-node @@network src))]
    (println "\n\nsaarland graph: nearest neighbour search with random src/dst")
    (println "B+ tree with:" (count (data/datoms @@network :eavt)) "nodes")
    (println "accuraccy: " (geometry/haversine src point) "meters")
    (c/quick-bench (:v (tool/nearest-node @@network src))
                   :os :runtime :verbose)))
