(ns service.routing.graph.specs
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [service.routing.graph.protocols :as rp]))

(s/def :arc/way int?)
(s/def :arc/dst int?)
(s/def :arc/src int?)

(s/def :graph/arc (s/and (s/keys :req-un [:arc/src :arc/dst :arc/way])
                         (fn [arc] (not= (:src arc) (:dst arc)))))

(s/def :node/lat (s/and number? #(<= -90 % 90)))
(s/def :node/lon (s/and number? #(<= -180 % 180)))
(s/def :node/arcs (s/map-of int? :graph/arc))

(s/def :graph/node (s/and (s/keys :req-un [:node/lat :node/lon :node/arcs])
                          #(not-empty (:arcs %))))
(s/def :int-map/graph (s/map-of int? :graph/node))

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
    (s/and :int-map/graph
           (partial valid-ids? ids)
           valid-arcs?)))
