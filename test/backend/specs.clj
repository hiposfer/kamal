(ns backend.specs
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [backend.routing.core :as core]))

(s/def :arc/kind (set (keys core/speeds)))
(s/def :arc/length (s/and int? pos?))
(s/def :arc/dst int?)
(s/def :arc/src int?)

(s/def :node/lat (s/and number? #(<= -90 % 90)))
(s/def :node/lon (s/and number? #(<= -180 % 180)))
(s/def :node/arcs (s/map-of int? :graph/arc))
(s/def :node/out-arcs :node/arcs)
(s/def :node/in-arcs :node/arcs)

(s/def :graph/arc (s/and (s/keys :req-un [:arc/src :arc/dst :arc/length :arc/kind])
                         (fn [arc] (not= (:src arc) (:dst arc)))))
(s/def :graph/node (s/and (s/keys :req-un [:node/lat :node/lon :node/out-arcs :node/in-arcs])
                          (fn [node] (or (not-empty (:out-arcs node)) (not-empty (:in-arcs node))))))
(s/def :int-map/graph (s/map-of int? :graph/node))

(defn- valid-ids?
  "is every id used in the out/in arcs also a node id present in the graph?"
  [ids graph]
  (let [outs (into #{} (comp (map :out-arcs) (map first)) (vals graph))
        ins  (into #{} (comp (map :in-arcs) (map first)) (vals graph))]
    (empty? (set/difference ids (set/union outs ins)))))

(defn- valid-out-arcs?
  [graph]
  (let [outs (map :out-arcs (vals graph))]
    (every? (fn [[dst arc]] (= dst (:dst arc))) outs)))

(defn- valid-in-arcs?
  [graph]
  (let [outs (map :in-arcs (vals graph))]
    (every? (fn [[src arc]] (= src (:src arc))) outs)))

;; this could definitely be improved such that every node/arc
;; is checked individually but this will have to do for the moment)))
(defn int-graph?
  "returns an anonymous spec to check the structure of this graph. Uses
  :int-map/graph as the base check"
  [graph]
  (let [ids (set (keys graph))]
    (s/and :int-map/graph
           (partial valid-ids? ids)
           valid-out-arcs?
           valid-in-arcs?)))