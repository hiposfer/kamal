(ns hiposfer.kamal.graph.algorithms
  (:require [hiposfer.kamal.graph.protocols :as rp]
            [hiposfer.kamal.graph.core :as route]
            [clojure.data.int-map :as imap]))

(defn dijkstra
  "returns a sequence of map-like entries which also implement the Traceable
   protocol. The key of a map entry is the node id and its value is
   a Valuable-protocol implementation returned by the cost function.
   In other words something similar to [id {:cost number :time number}]

  Parameters:
   - :value-by is a function that takes an Arc and a Trace
               and returns a Valuable from src to dst
   - :start-from is a set of node ids to start searching from
   - :direction is one of ::forward or ::backward and determines whether
     to use the outgoing or incoming arcs of each node"
  [graph & {:keys [start-from direction value-by]}]
  (let [direction (or direction ::forward)
        arcs (case direction ::forward  rp/successors
                             ::backward rp/predecessors)
        f    (case direction ::forward rp/dst
                             ::backward rp/src)]
    (route/->Dijkstra graph start-from value-by arcs f)))

(defn breath-first
  "returns a constant simple value of 1"
  [_ _] 1)

(defn components
  "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of an undirected graph"
  [undirected]
  (lazy-seq
    (when (not-empty undirected)
      (let [connected (sequence (map key) (dijkstra undirected
                                            :start-from #{(ffirst undirected)}
                                            :value-by breath-first))]
        (cons connected (components (apply dissoc undirected connected)))))))

;; note for specs: the biggest component of a biggest component should
;; be the passed graph (= graph (bc graph)) => true for all
(defn biggest-component
  "returns a subset of the original graph containing only the elements
  of the biggest strongly connected components"
  [graph]
  (let [subsets    (components graph)
        ids        (into (imap/int-set) (apply max-key count subsets))]
    (into (imap/int-map) (filter #(contains? ids (key %))) graph)))