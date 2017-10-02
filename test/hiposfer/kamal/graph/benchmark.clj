(ns hiposfer.kamal.graph.benchmark
  (:require [criterium.core :as c]
            [clojure.test :as test]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.graph.generators :as g]
            [hiposfer.kamal.graph.algorithms :as alg]
            [hiposfer.kamal.osm :as osm]
            [hiposfer.kamal.directions :as direction]
            [hiposfer.kamal.graph.protocols :as rp]))

(def iterations 10)

;; This is just to show the difference between a randomly generated graph
;; and a real-world graph. The randomly generated graph does not have a structure
;; meant to go from one place to the other, thus Dijkstra almost always fails to
;; finds a path (really fast)
(test/deftest ^:benchmark dijkstra-random-graph
  (let [graph        (gen/generate (g/graph 1000))
        sources      (into [] (repeatedly iterations #(rand-nth (keys graph))))
        destinations (into [] (repeatedly iterations #(rand-nth (keys graph))))]
    (println "\nrandomly generated graphs:" iterations "execution with random src/dst")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count rp/successors) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [i (range iterations)
                   :let [src (get sources i)
                         dst (get destinations i)
                         coll (alg/dijkstra graph
                                            :start-from #{src}
                                            :value-by (partial direction/duration graph))]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))

(def networker (delay (osm/osm->network "resources/osm/saarland.osm.bz2")))

(test/deftest ^:benchmark dijkstra-saarland-graph
  (let [graph        (:graph @networker) ;; force read
        sources      (into [] (repeatedly iterations #(rand-nth (keys graph))))
        destinations (into [] (repeatedly iterations #(rand-nth (keys graph))))]
    (println "\nsaarland graph:" iterations "executions with random src/dst")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count rp/successors) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [i (range iterations)
                   :let [src (get sources i)
                         dst (get destinations i)
                         coll (alg/dijkstra graph
                                            :start-from #{src}
                                            :value-by (partial direction/duration graph))]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))

;; only the connected nodes
(test/deftest ^:benchmark dijkstra-saarland-biggest-component
  (let [graph        (alg/biggest-component (:graph @networker))
        sources      (into [] (repeatedly iterations #(rand-nth (keys graph))))
        destinations (into [] (repeatedly iterations #(rand-nth (keys graph))))]
    (println "\nsaarland graph:" iterations "executions with random src/dst")
    (println "using only strongly connected components of the original graph")
    (println "dijkstra forward with:" (count graph) "nodes and"
             (reduce + (map (comp count rp/successors) (vals graph))) "edges")
    (c/quick-bench
      (dorun (for [i (range iterations)
                   :let [src (get sources i)
                         dst (get destinations i)
                         coll (alg/dijkstra graph
                                            :start-from #{src}
                                            :value-by (partial direction/duration graph))]]
               (reduce (fn [_ v] (when (= dst (key v)) (reduced v)))
                       nil
                       coll)))
      :os :runtime :verbose)))