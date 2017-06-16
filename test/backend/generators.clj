(ns backend.generators
  (:require [backend.routing.core :as core]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.set :as set]))

(s/def :arc/kind (set (keys backend.routing.core/speeds)))
(s/def :arc/length (s/and int? pos?))
(s/def :arc/dst int?)
(s/def :arc/src int?)

(s/def :node/lat (s/and number? #(<= -90 % 90)))
(s/def :node/lon (s/and number? #(<= -180 % 180)))
(s/def :node/arcs (s/map-of int? :graph/arc))
(s/def :node/out-arcs :node/arcs)
(s/def :node/in-arcs :node/arcs)

(s/def :graph/arc (s/keys :req-un [:arc/src :arc/dst :arc/length :arc/kind]))
(s/def :graph/node (s/keys :req-un [:node/lat :node/lon :node/out-arcs :node/in-arcs]))

(s/def :int-map/graph (s/map-of int? :graph/node))

(let [graph    (gen/generate (s/gen :int-map/graph))
      ids      (set (keys graph))
      out-ids  (into #{} (mapcat (comp set keys :out-arcs) (vals graph)))
      in-ids   (into #{} (mapcat (comp set keys :in-arcs) (vals graph)))]
  (println (count (set/difference (into out-ids in-ids) ids)))
  (set/intersection (into out-ids in-ids) ids))

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
  (let [picker #(gen/elements (range max))
        arc     (s/gen :graph/arc {:arc/src picker
                                   :arc/dst picker})
        arcs   #(gen/map (picker) arc)
        nodes   (s/gen :graph/node {:node/arcs arcs})]
    (gen/map (picker) nodes)))

(defn graph
  "generate a single graph with random integer node id in the 0 - max range"
  [max]
  (gen/fmap clean-graph (grapher max)))

;example usage
;(gen/generate (graph 10))