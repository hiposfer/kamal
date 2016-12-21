(ns hypobus.routing.core
  (:require [clojure.data.priority-map :as data]))
            ;[proto-repl.saved-values :as proto]))

(set! *warn-on-reflection* true)

(def graph {:a {:out {:b {:weight 7}
                      :c {:weight 9}
                      :f {:weight 14}}}
            :b {:out {:c {:weight 10}
                      :d {:weight 15}}}
            :c {:out {:d {:weight 11}
                      :f {:weight 2}}}
            :d {:out {:e {:weight 6}}}
            :e {:out {:f {:weight 9}}}
            :f {:out {}}})

(defn- relax-node
  "reducing function. Takes a cost-path a node id and its attributes and
  builds up a cost-path map of 'cheaper' routes"
  [prev-id prev-cost cost-path alts id attrs]
  (let [alternative (+ prev-cost (:weight attrs)) ;; from-to cost
        current     (or (:cost (get cost-path id)) Double/POSITIVE_INFINITY)] ;; current cost
    (when (> current alternative)
      (assoc alts id {:cost alternative :previous prev-id}))))

(defn path
  "uses the cost-path structure to reconstruct the path taken to reach the
  destination id"
  [cost-path dst-id]
  (reverse (take-while identity (iterate #(:previous (cost-path %)) dst-id))))

;;TODO:
;; - consider making it with transients/transducers
(defn dijkstra
  [graph src-id dst-id]
  (loop [cost-path {src-id {:cost 0 :previous nil}}
         queue     (data/priority-map-keyfn :cost src-id {:cost 0 :previous nil})]
    (let [[id cost]    (peek queue)
          edges        (:out  (get graph id))
          prev-cost    (:cost (get cost-path id))
          alternatives (reduce-kv #(relax-node id prev-cost cost-path %1 %2 %3)
                                  {} edges)]
      (if (nil? id) cost-path
        (recur (into cost-path alternatives)
               (into (pop queue) alternatives))))))
