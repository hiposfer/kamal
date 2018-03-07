(ns hiposfer.kamal.network.algorithms.core
  (:require [hiposfer.kamal.network.algorithms.dijkstra :as djk]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.set :as set]
            [datascript.core :as data]))

(defn dijkstra
  "returns a sequence of traversal-paths taken to reach each node. Each path is
   composed of [key value] pairs with key=node-id, value=total-cost.
  See Dijkstra algorithm

  Parameters:
   - graph: an collection of nodes over which the traversal will happen.
            Expected to be a Datascript db but need not be
   - value-by is a function that takes a (tempting) node-entity id and Trail
              and returns a Valuable implementation representing the weigth of traversing
              from the current node to the tempting one
   - successors: a function of id -> [ id ]. Used to get the id of the next nodes
   - start-from is a set of node-entity ids to start searching from

   NOTE: a Trail is a sequence of [id Valuable]. In other words it is the trail
         taken to get from the first settled node to the current one"
  [graph value-by successors start-from]
  (djk/->Dijkstra graph start-from value-by successors))

(defn breath-first
  "returns a sequence of traversal-paths taken to reach each node in the same
  manner as Dijkstra algorithm but with a constant cost.

  See Breath-first algorithm"
  [graph successors node-id]
  (dijkstra graph (constantly 1) successors #{node-id}))

(defn shortest-path
  "returns the path taken to reach dst using the provided graph traversal"
  [dst-id graph-traversal]
  (let [pred? (comp #{dst-id} key first)
        rf    (fn [_ value] (when (pred? value) (reduced value)))]
    (reduce rf nil graph-traversal)))

(defn node-ids [graph] (map :e (data/datoms graph :aevt :node/id)))

(defn- components
  "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of a undirected graph

   NOTE: only relevant for pedestrian routing"
  [graph settled]
  (if (= (count graph) (count settled)) (list)
    (let [start     (some #(and (not (settled %)) %)
                          (node-ids graph))
          connected (into #{} (comp (map first) (map key))
                              (breath-first graph tool/by-foot start))]
      (cons connected (lazy-seq (components graph (set/union settled connected)))))))

;; note for specs: the looner of the looner should be empty
(defn looners
  "returns a sequence of ids that can be removed from the graph
  because they are not part of the strongest connected component

  NOTE: only relevant for pedestrian routing"
  [graph]
  (let [subsets   (components graph #{})
        connected (apply max-key count subsets)]
    (remove #(contains? connected %) (node-ids graph))))
