(ns hiposfer.kamal.services.routing.graph
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [datascript.core :as data]))

(defn edge? [o] (and (satisfies? np/Arc o)
                     (satisfies? np/Bidirectional o)))

(defn arc? [o] (satisfies? np/Arc o))

(defn node? [o] (satisfies? np/Node o))

;; A bidirectional Arc. The bidirectionality is represented
;; through a mirror Arc, which is created as requested at runtime
(defrecord PedestrianEdge [^long src ^long dst ^long way]
  np/Arc
  (src [_] src)
  (dst [_] dst)
  np/Bidirectional
  (mirror [this]
    (if (:mirror? this)
      (dissoc this :mirror?) ; remove mirror from edge
      (map->PedestrianEdge {:src dst :dst src :way way :mirror? true})))
  (mirror? [this] (:mirror? this)))

;; A directed arc with an associated way; as per Open Street Maps
(defrecord Arc [^long src ^long dst]
  np/Arc
  (src [_] src)
  (dst [_] dst))

;; A node of a graph with a corresponding position using the World Geodesic
;; System. This implementation only accepts edges (bidirectional)
(defrecord PedestrianNode [^Long id location edges]
  np/Node
  (successors [_]
    (for [edge edges]
      (if (= id (np/dst edge)) edge
        (np/mirror edge)))))

(defrecord TransitStop [^Long id ^Double lat ^Double lon arcs edges]
  np/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon)
  np/Node
  (successors [this]
    (concat (:outgoing this)
            (for [edge edges]
              (if (= id (np/dst edge)) edge
                (np/mirror edge))))))

#_(let [network @(first @(:networks (:router hiposfer.kamal.dev/system)))]
    (for [node-id (data/datoms network :avet :node/id)
          :let [node (data/entity network (:e node-id))]]
      [(:e node-id) ()]))
