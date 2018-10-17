(ns hiposfer.kamal.services.routing.graph
  "functions to transform a Datascript representation to an Int-Map for
   super-fast lookups and Dijkstra routing"
  (:require [datascript.core :as data]
            [clojure.data.int-map :as i]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.network.algorithms.protocols :as np]))


(defn edge? [o] (and (satisfies? np/Arc o)
                     (satisfies? np/Bidirectional o)))

(defn arc? [o] (satisfies? np/Arc o))

(defn node? [o] (satisfies? np/Node o))

;; A bidirectional Arc. The bidirectionality is represented
;; through a mirror Arc, which is created as requested at runtime
(defrecord Edge [^long src ^long dst]
  np/Arc
  (src [_] src)
  (dst [_] dst)
  np/Bidirectional
  (mirror [this]
    (if (:mirror this)
      (dissoc this :mirror)
      (merge this {:src dst :dst src :mirror (not (:mirror this))})))
  (mirror? [this] (:mirror this)))

(defrecord Arc [^long src ^long dst]
  np/Arc
  (src [_] src)
  (dst [_] dst))

;; A node of a graph with a corresponding position using the World Geodesic
;; System. This implementation only accepts edges (bidirectional)
(defrecord PedestrianNode [^Long id location edges]
  np/GeoCoordinate
  (lat [_] (np/lat location))
  (lon [_] (np/lon location))
  np/Node
  (successors [_]
    (for [edge edges]
      (if (= id (np/dst edge))
        (np/mirror edge)
        edge))))

;; a Stop as per GTFS spec links is a sequence of Arc/Edges
(defrecord TransitStop [^Long id ^Double lat ^Double lon links]
  np/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon)
  np/Node
  (successors [_]
    (for [link links]
      (if (and (edge? link) (= id (np/dst link)))
        (np/mirror link)
        link))))

(defn- edge
  [entity]
  (map->Edge {:src   (:db/id (:edge/src entity))
              :dst   (:db/id (:edge/dst entity))
              :way/e (:db/id (:edge/way entity))}))

(defn- arc
  [entity]
  (map->Arc {:src     (:db/id (:arc/src entity))
             :dst     (:db/id (:arc/dst entity))
             :route/e (:db/id (:arc/route entity))}))

(defn- node-edges
  [network id]
  (map edge (concat (fastq/references network :edge/src id)
                    (fastq/references network :edge/dst id))))

(defn- stop-links
  [network id]
  (concat (map arc (fastq/references network :arc/src id))
          (map edge (fastq/references network :edge/dst id))))

(defn- nodes
  [network]
  (for [node-id (data/datoms network :avet :node/id)
        :let [node (data/entity network (:e node-id))]]
    [(:e node-id) (->PedestrianNode (:e node-id)
                                    (:node/location node)
                                    (node-edges network (:e node-id)))]))

(defn- stops
  [network]
  (for [stop-id (data/datoms network :avet :stop/id)
        :let [stop (data/entity network (:e stop-id))]]
    [(:e stop-id) (->TransitStop (:e stop-id)
                                 (:stop/lat stop)
                                 (:stop/lon stop)
                                 (stop-links network (:e stop-id)))]))

(defn create
  [network]
  (into (i/int-map)
        (concat (nodes network)
                (stops network))))

#_(time
    (let [network @(first @(:networks (:router hiposfer.kamal.dev/system)))]
      (last (::foo (assoc network ::foo (create network))))))

(defn osm-node? [o] (instance? PedestrianNode o))

(defn gtfs-stop? [o] (instance? TransitStop o))
