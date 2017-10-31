(ns hiposfer.kamal.graph.algorithms
  (:require [hiposfer.kamal.graph.protocols :as rp]
            [hiposfer.kamal.graph.core :as route]
            [clojure.data.int-map :as imap]
            [clojure.set :as set]))


(def movement
  "mapping of direction to protocol functions for Link and Nodes"
  {::forward  [rp/dst rp/successors]
   ::backward [rp/src rp/predecessors]})

(defn dijkstra
  "returns a sequence of traversal-paths taken to reach each node. Each path is
   composed of [key value] pairs with key=node-id, value=total-cost. See Dijkstra algorithm

  Parameters:
   - value-by is a function that takes an Arc and a Trace
               and returns a Valuable from src to dst
   - start-from is a set of node ids to start searching from
   - direction is one of ::forward or ::backward and determines whether
     to use the outgoing or incoming arcs of each node"
  ([value-by start-from graph]
   (dijkstra value-by start-from ::forward graph))
  ([value-by start-from direction graph]
   (let [[f arcs] (movement direction)]
     (route/->Dijkstra graph start-from value-by arcs f))))

(defn breath-first
  "returns a sequence of traversal-paths taken to reach each node in the same
  manner as Dijkstra algorithm but with a constant cost.

  See Breath-first algorithm"
  ([start-from graph]
   (breath-first start-from ::forward graph))
  ([start-from direction graph]
   (dijkstra (constantly 1) start-from direction graph)))

(defn shortest-path
  "returns the path taken to reach dst using the provided graph traversal"
  [dst-id graph-traversal]
  (let [pred? (comp #{dst-id} key first)
        rf    #(when (pred? %2) (reduced %2))]
    (reduce rf nil graph-traversal)))

(defn components
  "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of an undirected graph"
  [graph]
  (lazy-seq
    (when (not-empty graph)
      (let [connected (sequence (comp (map first) (map key))
                        (breath-first #{(ffirst graph)} graph))
            new-graph (reduce route/detach graph connected)]
        (cons connected (components new-graph))))))

;; note for specs: the biggest component of a biggest component should
;; be the passed graph (= graph (bc graph)) => true for all
(defn biggest-component
  "returns a subset of the original graph containing only the elements
  of the biggest strongly connected components"
  [undirected-graph]
  (let [subsets   (components undirected-graph)
        connected (into (imap/int-set) (apply max-key count subsets))
        ids       (into (imap/int-set) (keys undirected-graph))]
    (reduce route/detach
            undirected-graph
            (set/difference ids connected))))