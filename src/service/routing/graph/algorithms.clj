(ns service.routing.graph.algorithms
  (:require [service.routing.graph.protocols :as rp]
            [service.routing.graph.core :as route]
            [clojure.data.int-map :as imap]))

(defn dijkstra
  "returns a sequence of map-like entries which also implement the Traceable
   protocol. The key of a map entry is the node id and its value is
   a Valuable-protocol implementation returned by the cost function.
   In other words something similar to [id {:cost number :time number}]

  Parameters:
   - :value-by is a function that takes an Arc and an Identifiable Trace
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
  "returns a constant SimpleValue of 1"
  [_ _] (route/->SimpleValue 1))

(defn- unreachable
  "returns a node's id whenever a node doesnt have any out nor in arcs,
  nil otherwise"
  [[id node]]
  (when (and (empty? (:out-arcs node)) (empty? (:in-arcs node)))
    id))

(defn- remove-loners
  "disassociate every node from graph that is unreachable"
  [graph]
  (let [removable (sequence (comp (map unreachable) (remove nil?)) graph)]
    (persistent! (reduce dissoc! (transient graph) removable))))

(defn- reflect-arcs
  "returns a graph where all outgoing arcs from id are reflected into
  its successors

  NOTE: it currently depends on having :out-arcs, :dst and :src as part of
  graph and node. Implementation details leakage :/"
  [graph id]
  (reduce (fn [graph edge] (update-in graph [(:dst edge) :out-arcs]
                                      conj (route/->Arc (:dst edge) (:src edge)
                                                        (:length edge) (:kind edge))))
          graph
          (rp/successors (get graph id))))

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
  of the biggest weakly connected components"
  [graph]
  (let [cgraph     (remove-loners graph)
        undirected (reduce-kv (fn [res id _] (reflect-arcs res id))
                              cgraph
                              cgraph)
        subsets    (components undirected)
        ids        (into (imap/int-set) (apply max-key count subsets))]
    (into (imap/int-map) (filter #(contains? ids (key %))) cgraph)))


;(def grapher (time (biggest-component (time (osm/osm->graph "resources/osm/saarland.osm")))))

;; NOTE: for testing purposes only
;(def rosetta {1 {:out-arcs [{:dst 2 :length 7  :kind :other}
;                            {:dst 3 :length 9  :kind :other}
;                            {:dst 6 :length 14 :kind :other}]
;                 :in-arcs  nil}
;              2 {:out-arcs [{:dst 3 :length 10 :kind :other}
;                            {:dst 4 :length 15 :kind :other}]
;                 :in-arcs  [{:src 1 :length 7  :kind :other}]}
;              3 {:out-arcs [{:dst 4 :length 11 :kind :other}
;                            {:dst 6 :length 2  :kind :other}]
;                 :in-arcs  [{:src 1 :length 9  :kind :other}
;                            {:src 2 :length 10 :kind :other}]}
;              4 {:out-arcs [{:dst 5 :length 6  :kind :other}]
;                 :in-arcs  [{:src 2 :length 15 :kind :other}
;                            {:src 3 :length 11 :kind :other}]}
;              5 {:out-arcs [{:dst 6 :length 9  :kind :other}]
;                 :in-arcs  [{:src 4 :length 6  :kind :other}]}
;              6 {:out-arcs nil
;                 :in-arcs  [{:src 1 :length 14 :kind :other}
;                            {:src 3 :length 2  :kind :other}
;                            {:src 5 :length 9  :kind :other}]}})
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

;(def performer (dijkstra rosetta
;                         :value-by service.routing.directions/length
;                         :direction ::forward
;                         :start-from #{1}))
;(first performer)
;(second performer)
;(nth performer 2)
;(map identity performer)
;; ------- examples
;(reduce
;  (fn [res v]
;    (if-not (nil? res)
;      (println (map route/id (route/path res))
;               (map route/id (route/path v)))
;      (println res (map route/id (route/path v)))))
;  performer)
;
;(reduce
;  (fn [res v] (println res (map route/id (route/path v))))
;  nil
;  performer)
;
;
;(map (comp (partial map route/id) route/path) performer)
;(map route/id (route/path (nth performer 6)))

;(transduce (halt-when (fn bar [v] (= 4 (:id v))))
;           (fn foo [_ v] (println "id" (:id v)))
;           nil
;           performer)