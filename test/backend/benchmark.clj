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

(def iterations 10)

;; This is just to show the difference between a randomly generated graph
;; and a real-world graph. The randomly generated graph does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph (gen/generate (g/graph 1000))
        sources      (into [] (repeatedly iterations #(rand-nth (keys graph))))
        destinations (into [] (repeatedly iterations #(rand-nth (keys graph))))]
    (println "\nrandomly generated graphs:" iterations "execution with random src/dst")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count :out-arcs) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [i (range iterations)
                   :let [src (get sources i)
                         dst (get destinations i)
                         coll (alg/dijkstra graph :start-from #{src}
                                                  :direction ::alg/forward
                                                  :value-by alg/length)]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))

(def grapher (delay (osm/osm->graph "resources/osm/saarland.osm")))

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [graph @grapher ;; force read
        sources      (into [] (repeatedly iterations #(rand-nth (keys graph))))
        destinations (into [] (repeatedly iterations #(rand-nth (keys graph))))]
    (println "\nsaarland graph:" iterations "executions with random src/dst")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count :out-arcs) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [i (range iterations)
                   :let [src (get sources i)
                         dst (get destinations i)
                         coll (alg/dijkstra graph :start-from #{src}
                                                   :direction ::alg/forward
                                                   :value-by alg/length)]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))

(test/deftest ^:benchmark dijkstra-saarland-biggest-component
  (let [graph (alg/biggest-component @grapher) ;; only the connected nodes
        sources      (into [] (repeatedly iterations #(rand-nth (keys graph))))
        destinations (into [] (repeatedly iterations #(rand-nth (keys graph))))]
    (println "\nsaarland graph:" iterations "executions with random src/dst")
    (println "using only weakly connected components of the original graph")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count :out-arcs) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [i (range iterations)
                   :let [src (get sources i)
                         dst (get destinations i)
                         coll (alg/dijkstra graph :start-from #{src}
                                             :direction ::alg/forward
                                             :value-by alg/length)]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))