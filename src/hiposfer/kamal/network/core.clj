(ns hiposfer.kamal.network.core
  "An implementation of the Graph protocols oriented towards
  Road Networks. See Graph namespace for more information"
  (:require [hiposfer.kamal.network.graph.protocols :as gp]
            [hiposfer.kamal.network.algorithms.protocols :as np]
            [hiposfer.kamal.network.graph.core :as graph])
  (:import (clojure.lang APersistentMap IPersistentVector)))

;; -------------------------------
; network is an {id node}
; NodeInfo is a {:lon :lat :links {node-id arc}}
; Arc is a {:src :dst :way-id}

;; A bidirectional Arc. The bidirectionality is represented
;; through a mirror Arc, which is created as requested at runtime
(defrecord Edge [^long src ^long dst ^long way]
  gp/Link
  (src [_] src)
  (dst [_] dst)
  gp/Bidirectional
  (mirror [this]
    (if (:mirror? this)
      (dissoc this :mirror?) ; remove mirror from edge
      (map->Edge {:src dst :dst src :way way :mirror? true})))
  (mirror? [this] (:mirror? this)))

;; A directed arc with an associated way; as per Open Street Maps
(defrecord Arc [^long src ^long dst]
  gp/Link
  (src [_] src)
  (dst [_] dst))

;; A node of a graph with a corresponding position using the World Geodesic
;; System. This implementation only accepts one Link per src/dst node.
;; Edges are stored alongside Arcs according to their src/dst value
(defrecord NodeInfo [^double lon ^double lat outgoing incoming]
  gp/Context
  (predecessors [this] (concat (sequence (comp (filter graph/edge?)
                                               (map gp/mirror))
                                         (vals (:outgoing this)))
                               (vals (:incoming this))))
  (successors   [this] (concat (sequence (comp (filter graph/edge?)

                                               (map gp/mirror))
                                         (vals (:incoming this)))
                               (vals (:outgoing this))))
  np/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon)
  gp/Binder
  (inbound  [this arc-or-edge]
    (assoc-in this [:incoming (gp/src arc-or-edge)] arc-or-edge))
  (outbound [this arc-or-edge]
    (assoc-in this [:outgoing (gp/dst arc-or-edge)] arc-or-edge))
  gp/Incoherent
  (disconnect [this arc-or-edge]
    (-> (update this :outgoing dissoc (gp/dst arc-or-edge))
        (update :incoming dissoc (gp/src arc-or-edge)))))

;; a Point is a simple longitude, latitude pair used to
;; represent the geometry of a way in Open Street Maps
(defrecord Point [^double lon ^double lat]
  np/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon))

;; A vector of two numbers can be interpreted as a Point
;; according to the GeoJson standard
(extend-type IPersistentVector
  np/GeoCoordinate
  (lat [this] (second this))
  (lon [this] (first this)))

;; A map can be used to represent Nodes of a graph, such that it avoids
;; using specialized implementations. This is specially useful for
;; generative testing: there we use generated nodes and arcs
(extend-type APersistentMap
  gp/Context
  (predecessors [this] (concat (sequence (comp (filter graph/edge?)
                                               (map gp/mirror))
                                         (vals (:outgoing this)))
                               (vals (:incoming this))))
  (successors   [this] (concat (sequence (comp (filter graph/edge?)
                                               (map gp/mirror))
                                         (vals (:incoming this)))
                               (vals (:outgoing this))))
  np/GeoCoordinate
  (lat [this] (:lat this))
  (lon [this] (:lon this))
  gp/Binder
  (inbound  [this arc-or-edge]
    (assoc-in this [:incoming (gp/src arc-or-edge)] arc-or-edge))
  (outbound [this arc-or-edge]
    (assoc-in this [:outgoing (gp/dst arc-or-edge)] arc-or-edge))
  gp/Incoherent
  (disconnect [this arc-or-edge]
    (-> (update this :outgoing dissoc (gp/dst arc-or-edge))
        (update :incoming dissoc (gp/src arc-or-edge))))
  ;; arc representation
  gp/Link
  (src [this] (:src this))
  (dst [this] (:dst this)))

;; A Number is the simplest way to represent the cost of traversing an Arc
;; in a Graph. Useful for Dijkstra and similar algorithms
(extend-type Number
  np/Valuable
  (cost [this] this)
  (sum  [this that] (+ this that)))
