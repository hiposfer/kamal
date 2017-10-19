(ns hiposfer.kamal.graph.algorithms
  (:require [hiposfer.kamal.graph.protocols :as rp]
            [hiposfer.kamal.graph.core :as route]
            [clojure.data.int-map :as imap]
            [clojure.set :as set]))

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

(def breath-first (constantly 1))

(defn components
  "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of an undirected graph"
  [graph]
  (lazy-seq
    (when (not-empty graph)
      (let [connected (sequence (comp (map first) (map key))
                        (dijkstra graph
                          :start-from #{(ffirst graph)}
                          :value-by breath-first))
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
        ids       (into #{} (keys undirected-graph))]
    (reduce route/detach
            undirected-graph
            (set/difference ids connected))))