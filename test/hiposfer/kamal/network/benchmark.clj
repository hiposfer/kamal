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
  (let [dt     (gen/generate (ng/graph 1000))
        conn   (data/create-conn router/schema)
        _      (data/transact! conn dt)
        src    (rand-nth (alg/node-ids @conn))
        dst    (rand-nth (alg/node-ids @conn))]
    (println "\n\nDIJKSTRA forward with:" (count (alg/node-ids @conn)) "nodes")
    (println "**random graph")
    (c/quick-bench
      (let [coll (alg/dijkstra @conn #(directions/duration @conn %1 %2)
                                      tool/by-foot
                                      #{src})]
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
  (let [graph   @networker ;; force read
        ;; the src and dst might have been removed from the original network
        src     (rand-nth (alg/node-ids graph))
        dst     (rand-nth (alg/node-ids graph))]
    (println "\n\nDIJKSTRA forward with:" (count (alg/node-ids graph)) "nodes")
    (println "saarland graph:")
    (c/quick-bench
      (let [coll (alg/dijkstra graph #(directions/duration graph %1 %2)
                                      tool/by-foot
                                      #{src})]
        (alg/shortest-path dst coll))
      :os :runtime :verbose)))

;; note src nil search will search for points greater or equal to src
;; I think nil src then search points less than src
(test/deftest ^:benchmark nearest-neighbour-search
  (let [src   [7.038535 49.345088]
        point (:v (tool/nearest-node @networker src))]
    (println "\n\nsaarland graph: nearest neighbour search with random src/dst")
    (println "B+ tree with:" (count (data/datoms @networker :eavt)) "nodes")
    (println "accuraccy: " (geometry/haversine src point))
    (c/quick-bench (:v (tool/nearest-node @networker src))
                   :os :runtime :verbose)))
