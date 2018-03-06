(ns hiposfer.kamal.network.algorithms.core
  (:require [hiposfer.kamal.network.algorithms.dijkstra :as djk]
            [hiposfer.kamal.network.graph.core :as graph]
            [clojure.set :as set]))

(defn dijkstra
  "returns a sequence of traversal-paths taken to reach each node. Each path is
   composed of [key value] pairs with key=node-id, value=total-cost. See Dijkstra algorithm

  Parameters:
   - value-by is a function that takes an Arc and a Trace
               and returns a Valuable from src to dst
   - start-from is a set of node ids to start searching from"
  [graph value-by successors start-from]
  (djk/->Dijkstra graph start-from value-by successors))

(defn breath-first
  "returns a sequence of traversal-paths taken to reach each node in the same
  manner as Dijkstra algorithm but with a constant cost.

  See Breath-first algorithm"
  ([graph node-id]
   (breath-first graph ::forward node-id))
  ([graph direction node-id]
   (dijkstra graph direction (constantly 1) #{node-id})))

(defn shortest-path
  "returns the path taken to reach dst using the provided graph traversal"
  [dst-id graph-traversal]
  (let [pred? (comp #{dst-id} key first)
        rf    (fn [_ value] (when (pred? value) (reduced value)))]
    (reduce rf nil graph-traversal)))

(defn- components
  "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of an undirected graph"
  [graph settled]
  (if (= (count graph) (count settled)) (list)
    (let [start     (some #(and (not (settled %)) %) (keys graph))
          connected (into #{} (comp (map first) (map key))
                              (breath-first graph start))]
      (cons connected (lazy-seq (components graph (set/union settled connected)))))))

;; note for specs: the biggest component of a biggest component should
;; be the passed network (= network (bc network)) => true for all
(defn biggest-component
  "returns a subset of the original graph containing only the elements
  of the biggest strongly connected components"
  [undirected-graph]
  (let [subsets   (components undirected-graph #{})
        connected (apply max-key count subsets)]
    (transduce (remove connected)
               (completing graph/detach)
               undirected-graph
               (keys undirected-graph))))
