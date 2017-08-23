(ns service.routing.graph.generators
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [service.routing.graph.specs])) ;; loads the spec in the registry

(defn- clean-arcs
  "remove the arcs of node that point to unexistent nodes and fix the src id
  to the one from the node"
  [graph [id node]]
  (let [arcs (sequence (comp (filter (fn [[dst _]] (and (contains? graph dst))))
                             (map    (fn [[dst arc]] [dst (assoc arc :src id)]))
                             (map    (fn [[dst arc]] (if (not= dst (:src arc))
                                                       [dst arc]
                                                       (let [new-dst (rand-nth (keys graph))]
                                                         [new-dst (assoc arc :dst new-dst)])))))
                       (:arcs node))]
    [id (assoc node :arcs (into {} arcs))]))

(defn- fix-dst
  "set the node id of :arcs to dst"
  [[id node]]
  (let [arcs (map (fn [[_ arc]] [(:dst arc) arc])
                  (:arcs node))]
    [id (assoc node :arcs (into {} arcs))]))

(defn- clean-graph
  "remove all arcs that point to unexistent nodes"
  [graph]
  (let [graph2 (into {} (map fix-dst graph))]
    (into {} (map #(clean-arcs graph2 %) graph2))))

(defn- grapher
  "returns a graph generator. The generated graph might not be consistent
   since its node ids are randomly generated"
  [max]
  (let [picker #(gen/elements (range (* 3 max)))
        arc     (s/gen :graph/arc {:arc/src picker
                                   :arc/dst picker})
        arcs   #(gen/map (picker) arc {:min-elements 3 :max-elements 5})
        nodes   (s/gen :graph/node {:node/arcs arcs})]
    (gen/map (picker) nodes {:num-elements max, :max-tries 100})))

(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap clean-graph (grapher size)))

;example usage
(gen/generate (graph 10))