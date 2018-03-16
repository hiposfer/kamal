(ns hiposfer.kamal.network.algorithms.core
  (:require [hiposfer.kamal.network.algorithms.dijkstra :as djk]
            [hiposfer.kamal.libs.fastq :as fastq]
            [clojure.set :as set]
            [datascript.core :as data]))

(def defaults {:value-by (constantly 1)
               :comparator compare})

(defn dijkstra
  "returns a sequence of traversal-paths taken to reach each node. Each path is
   composed of [key value] pairs with key=node-id, value=total-cost.
  See Dijkstra algorithm

  Parameters:
   - graph: an collection of nodes over which the traversal will happen.
            Expected to be a Datascript db but need not be
   - start-from is a set of node-entity ids to start searching from
   - opts: is a map of options with the following keys
     :successors -> a function of graph, id -> [ id ]. Used to get the id of
                    the next nodes value-by successors
     :value-by -> a function of graph, node , trail -> cost
     :comparator -> a comparator function as defined by Clojure and Java.
                    defaults to clojure/compare
   NOTE: a Trail is a sequence of [id Valuable]. In other words it is the trail
         taken to get from the first settled node to the current one"
  [graph start-from opts]
  (let [opts (merge defaults opts)]
    (djk/->Dijkstra graph start-from (:value-by opts)
                                     (:successors opts)
                                     (:comparator opts))))

(defn shortest-path
  "returns the path taken to reach dst using the provided graph traversal"
  [dst graph-traversal]
  (let [dst? (comp #{dst} key first)
        rf   (fn [_ value] (when (dst? value) (reduced value)))]
    (reduce rf nil graph-traversal)))

(defn nodes
  "returns all the node entities in the network"
  [network]
  (sequence (comp (map :e)
                  (map #(data/entity network %)))
            (data/datoms network :aevt :node/id)))

(defn- components
  "returns a lazy sequence of sets of nodes' ids of each strongly connected
   component of a undirected graph

   NOTE: only relevant for pedestrian routing"
  [network settled]
  (if (= (count (nodes network)) (count settled)) (list)
    (let [start     (some #(and (not (settled %)) %)
                           (nodes network))
          djks (breath-first network tool/node-successors start)
          connected (sequence (comp (map first) (map key)) djks)]
     (cons connected
           (lazy-seq (components network (into settled connected)
                          (node-ids network)))
          connected (into #{} (comp (map first) (map key))
                          (dijkstra network #{start} {:successors fastq/node-successors}))
      (cons connected (lazy-seq (components network (set/union settled connected))))))))

;; note for specs: the looner of the looner should be empty
(defn looners
  "returns a sequence of ids that can be removed from the graph
  because they are not part of the strongest connected component

  NOTE: only relevant for pedestrian routing"
  [network]
  (let [subsets   (components network #{})
        connected (into #{} (apply max-key count subsets))]
    (remove #(contains? connected %) (nodes network))))
