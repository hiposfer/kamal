(ns service.routing.algorithms
  (:require [service.utils.tool :as utils]
            [service.routing.osm :as osm]
            [service.routing.protocols :as rp]
            [service.routing.core :as route]))

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
  (let [arcs (condp = direction ::forward  rp/successors
                                ::backward rp/predecessors)]
    (route/->Dijkstra graph start-from value-by arcs)))

(defn length
  "A very simple value computation function for Arcs in a graph.
  Returns a SimpleValue with the length of the arc"
  [arc _]
  (let [val (/ (:length arc) (get (:kind arc) osm/speeds osm/min-speed))]
    (route/->SimpleValue val)))

(defn- reflect-arcs
  "returns a graph where all outgoing arcs from id are reflected to into
  its successors

  NOTE: it currently depends on having :out-arcs, :dst and :src as part of
  graph and node. Implementation details leakage :/"
  [graph id]
  (reduce-kv (fn [graph dst edge] (assoc-in graph [dst :out-arcs id]
                                            (assoc edge :dst id :src dst)))
             graph
             (rp/successors (get graph id))))

(defn components
  "returns a sequence of sets of nodes' ids of each weakly connected component of a graph"
  [graph]
  (let [undirected (reduce-kv (fn [res id _] (reflect-arcs res id))
                              graph
                              graph)]
    (loop [remaining-graph undirected
           result          []]
      (let [component   (sequence (map key)
                                  (dijkstra undirected :start-from #{(ffirst remaining-graph)}
                                                  :direction ::route/forward, :value-by length))
            next-result (conj result component)
            next-graph  (apply dissoc remaining-graph component)]
        (if (empty? next-graph)
          next-result
          (recur next-graph next-result))))))

;; note for specs: the biggest component of a biggest component should
;; be the passed graph (= graph (bc graph)) => true for all
(defn biggest-component
  "returns a subset of the original graph containing only the elements
  of the biggest weakly connected components"
  [graph]
  (let [subsets (components graph)
        ids     (apply max-key count subsets)]
    (select-keys graph ids)))

;(def grapher (osm/cleanup (osm/osm->graph "resources/osm/saarland.osm")))

;(time (count (biggest-component grapher)))

;(let [graph (biggest-component (gen/generate (g/graph 10)))]
;  (= (biggest-component graph) graph))

;(biggest-component rosetta)

(defn path
  "uses the traces structure to reconstruct the path taken to reach the
  destination id"
  [graph traces dst-id]
  (let [reids (iterate #(:previous (get traces %)) dst-id)
        ids   (reverse (take-while identity reids))]
    (map #(get graph %) ids)))

(defn brute-nearest
  "search the nearest node in graph to point using the euclidean function"
  [graph point]
  (println (:lat point) (:lon point))
  (reduce-kv (fn [best _ node] (min-key #(utils/euclidean-pow2 point %) node best))
             (second (first graph)) graph))


;; NOTE: for testing purposes only
;(def rosetta {1 {:out-arcs {2 {:dst 2 :length 7  :kind :other}
;                            3 {:dst 3 :length 9  :kind :other}
;                            6 {:dst 6 :length 14 :kind :other}}
;                 :in-arcs  {}}
;              2 {:out-arcs {3 {:dst 3 :length 10 :kind :other}
;                            4 {:dst 4 :length 15 :kind :other}}
;                 :in-arcs  {1 {:src 1 :length 7  :kind :other}}}
;              3 {:out-arcs {4 {:dst 4 :length 11 :kind :other}
;                            6 {:dst 6 :length 2  :kind :other}}
;                 :in-arcs  {1 {:src 1 :length 9  :kind :other}
;                            2 {:src 2 :length 10 :kind :other}}}
;              4 {:out-arcs {5 {:dst 5 :length 6  :kind :other}}
;                 :in-arcs  {2 {:src 2 :length 15 :kind :other}
;                            3 {:src 3 :length 11 :kind :other}}}
;              5 {:out-arcs {6 {:dst 6 :length 9  :kind :other}}
;                 :in-arcs  {4 {:src 4 :length 6  :kind :other}}}
;              6 {:out-arcs {}
;                 :in-arcs  {1 {:src 1 :length 14 :kind :other}
;                            3 {:src 3 :length 2  :kind :other}
;                            5 {:src 5 :length 9  :kind :other}}}})
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

;(dijkstra rosetta (route/->ArcLengthRouter 1 5 ::route/forward))

;(def performer (route/dijkstra rosetta :worth length
;                                       :direction ::route/forward
;                                       :src   #{1}))
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