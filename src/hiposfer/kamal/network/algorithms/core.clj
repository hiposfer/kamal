(ns hiposfer.kamal.network.algorithms.core
  (:require [hiposfer.kamal.network.algorithms.dijkstra :as djk]
            [datascript.core :as data]))

(defn dijkstra
  "returns a sequence of traversal-paths taken to reach each node

  Parameters:
   - router: an implementation of the Router protocol to direct the 'movement'
      of the graph traversal
   - start-from: is a set of either
      - entities to start searching from
      - [entity init] pair where init is value to start settling nodes
   - comparator: a standard java.util.comparator implementation to compare the
      values returned by the router. Defaults to nil, which means that Valuable
      implementation MUST be comparable i.e. implement java.util.Comparable"
  ([router start-from]
   (dijkstra router start-from nil))
  ([router start-from comparator]
   (djk/->Dijkstra router start-from comparator)))

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
  [network router settled]
  (if (= (count (nodes network)) (count settled)) (list)
    (let [start     (some #(and (not (settled %)) %)
                           (nodes network))
          connected (sequence (comp (map first) (map key))
                              (dijkstra router #{start}))]
     (cons connected (lazy-seq (components network router (into settled connected)))))))

;; note for specs: the looner of the looner should be empty
(defn looners
  "returns a sequence of ids that can be removed from the graph
  because they are not part of the strongest connected component

  NOTE: only relevant for pedestrian routing"
  [network router]
  (let [subsets   (components network router #{})
        connected (into #{} (apply max-key count subsets))]
    (remove #(contains? connected %) (nodes network))))
