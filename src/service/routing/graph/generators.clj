(ns service.routing.graph.generators
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [service.routing.graph.specs])) ;; loads the spec in the registry

(defn- clean-arcs
  "remove the arcs of node that point to unexistent nodes"
  [graph [id node]]
  (let [outs (filter (fn [[dst _]] (contains? graph dst))
                     (:out-arcs node))
        ins  (filter (fn [[src _]] (contains? graph src))
                     (:in-arcs node))]
    [id (assoc node :out-arcs (into {} outs)
                    :in-arcs  (into {} ins))]))

(defn- reflect-arcs
  "set the node id of :out-arcs and :in-arcs to src or dst
  accordingly"
  [[id node]]
  (let [outs (map (fn [[_ arc]] [(:dst arc) arc])
                  (:out-arcs node))
        ins  (map (fn [[_ arc]] [(:src arc) arc])
                  (:in-arcs node))]
    [id (assoc node :out-arcs (into {} outs)
                    :in-arcs  (into {} ins))]))

(defn- clean-graph
  "remove all arcs that point to unexistent nodes"
  [graph]
  (let [graph2 (into {} (map reflect-arcs graph))]
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
;(gen/generate (graph 10))