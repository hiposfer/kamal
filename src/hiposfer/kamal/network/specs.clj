(ns hiposfer.kamal.network.specs
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.network.graph.protocols :as rp]
            [hiposfer.geojson.specs :as geojson]))

(s/def ::way int?)
(s/def ::dst int?)
(s/def ::src int?)

(s/def ::arc (s/keys :req-un [::src ::dst ::way]))
(s/def ::arcs (s/map-of int? ::arc))

(s/def ::node (s/keys :req-un [::geojson/lat ::geojson/lon ::arcs]))
(s/def ::graph (s/map-of int? ::node))

;; todo: also need to validate src id since it can be reversed
(defn- valid-arcs?
  [graph]
  (let [outs (map :arcs (vals graph))]
    (every? (fn [[dst arc]] (= dst (:dst arc))) outs)))

(defn- valid-ids?
  "is every id used in the out arcs also a node id present in the graph?"
  [ids graph]
  (let [outs (into #{} (comp (map :arcs)
                             (map rp/dst))
                       (vals graph))]
    (empty? (set/difference ids outs))))

;; this could definitely be improved such that every node/arc
;; is checked individually but this will have to do for the moment)))
(defn int-graph?
  "returns an anonymous spec to check the structure of this graph. Uses
  :int-map/graph as the base check"
  [graph]
  (let [ids (set (keys graph))]
    (s/and ::graph
           (partial valid-ids? ids)
           valid-arcs?)))
