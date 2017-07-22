(ns service.routing.graph.generators
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [service.routing.graph.specs])) ;; loads the spec in the registry

(defn- clean-arcs
  [graph [id node]]
  (let [outs (filter #(contains? graph (:dst %)) (:out-arcs node))
        ins  (filter #(contains? graph (:src %)) (:in-arcs node))]
    [id (assoc node :out-arcs outs
                    :in-arcs  ins)]))

(defn- clean-graph
  [graph]
  (into (sorted-map) (map #(clean-arcs graph %) graph)))

(defn- grapher
  "returns a graph generator. WARNING: do not use this directly !!
  The generated graph might not be consistent since its node ids are
  randomly generated"
  [max]
  (let [picker #(gen/elements (range (* 3 max)))
        arc     (s/gen :graph/arc {:arc/src picker
                                   :arc/dst picker})
        arcs   #(gen/list-distinct arc {:min-elements 3 :max-elements 5})
        nodes   (s/gen :graph/node {:node/arcs arcs})]
    (gen/map (picker) nodes {:num-elements max, :max-tries 100})))

(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap clean-graph (grapher size)))

;example usage
;(gen/generate (graph 10))
