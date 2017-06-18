(ns backend.generators
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [backend.specs :as core.specs])) ;; loads the spec in the registry

(defn- reflect-arcs
  [[id node]]
  (let [outs (map #(vector (:dst (second %)) (second %)) (:out-arcs node))
        ins  (map #(vector (:src (second %)) (second %)) (:in-arcs node))]
    [id (assoc node :out-arcs (into {} outs)
                    :in-arcs  (into {} ins))]))

(defn- clean-arcs
  [graph [id node]]
  (let [outs (filter #(contains? graph (first %)) (:out-arcs node))
        ins  (filter #(contains? graph (first %)) (:in-arcs node))]
    [id (assoc node :out-arcs (into {} outs)
                    :in-arcs  (into {} ins))]))

(defn- clean-graph
  [graph]
  (let [graph2 (into {} (map reflect-arcs graph))]
    (into (sorted-map) (map #(clean-arcs graph2 %) graph2))))

(defn- grapher
  "returns a graph generator. WARNING: do not use this directly !!
  The generated graph might not be consistent since its node ids are
  randomly generated"
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