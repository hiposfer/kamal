(ns backend.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [backend.generators :as g]
            [clojure.spec.gen.alpha :as gen]
            [backend.routing.core :as core]
            [backend.routing.algorithms :as alg]
            [backend.routing.osm :as osm]))

;; NOTE: to execute this algorithm you need to decompress the bz2 file
;; in the resources/osm dir !!

;; This is just to show the difference between a randomly generated graph
;; and a real-world graph. The randomly generated graph does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph (gen/generate (g/graph 1000))]
    (println "\nrandomly generated graphs: 10 execution with random src/dst")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count :out-arcs) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [_ (range 10)
                   :let [src (rand-nth (keys graph))
                         dst (rand-nth (keys graph))
                         coll (core/dijkstra graph :start-from #{src}
                                                   :direction ::core/forward
                                                   :value-by alg/length)]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))

(def grapher (delay (osm/cleanup (osm/osm->graph "resources/osm/saarland.osm"))))

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [graph @grapher] ;; force read
    (println "\nsaarland graph: 10 executions with random src/dst")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count :out-arcs) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [_ (range 10)
                   :let [src (rand-nth (keys graph))
                         dst (rand-nth (keys graph))
                         coll (core/dijkstra graph :start-from #{src}
                                                   :direction ::core/forward
                                                   :value-by alg/length)]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))

(test/deftest ^:benchmark dijkstra-saarland-biggest-component
  (let [graph (alg/biggest-component @grapher)] ;; only the connected nodes
    (println "\nsaarland graph: 10 executions with random src/dst")
    (println "using only weakly connected components of the original graph")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count :out-arcs) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [_ (range 10)
                   :let [src (rand-nth (keys graph))
                         dst (rand-nth (keys graph))
                         coll (core/dijkstra graph :start-from #{src}
                                             :direction ::core/forward
                                             :value-by alg/length)]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))