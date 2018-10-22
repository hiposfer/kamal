(ns hiposfer.kamal.router.graph
  "functions to transform a Datascript representation to an Int-Map for
   super-fast lookups and Dijkstra routing"
  (:require [datascript.core :as data]
            [clojure.data.int-map :as i]
            [hiposfer.kamal.router.util.fastq :as fastq]
            [hiposfer.kamal.router.algorithms.protocols :as np]))


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

(defn- nodes
  [network edges-from edges-to]
  (for [datom (data/datoms network :avet :node/id)
        :let [node (data/entity network (:e datom))]]
    [(:e datom) (->PedestrianNode (:e datom)
                                  (:node/location node)
                                  (concat (get edges-from (:e datom))
                                          (get edges-to (:e datom))))]))

(defn- stops
  [network edges-to arcs-from]
  (for [datom (data/datoms network :avet :stop/id)
        :let [stop (data/entity network (:e datom))]]
    [(:e datom) (->TransitStop (:e datom)
                               (:stop/lat stop)
                               (:stop/lon stop)
                               (concat (get edges-to (:e datom))
                                       (get arcs-from (:e datom))))]))

(defn- edges-tx
  [network]
  (for [datom (data/datoms network :aevt :way/nodes)
        :let [nodes (:v datom)
              way   (data/entity network (:e datom))]
        [from to] (map vector nodes (rest nodes))]
    {:edge/src (:db/id (data/entity network [:node/id from]))
     :edge/dst (:db/id (data/entity network [:node/id to]))
     :edge/way (:db/id (data/entity network [:way/id (:way/id way)]))}))

(defn create
  [network]
  (let [db     (as-> (time (data/db-with network (edges-tx network))) db
                     (time (data/db-with db (fastq/link-stops db)))
                     (time (data/db-with db (fastq/cache-stop-successors db))))
        edges  (for [datom (data/datoms db :aevt :edge/src)]
                 (edge (data/entity db (:e datom))))
        arcs   (for [datom (data/datoms db :aevt :arc/src)]
                 (arc (data/entity db (:e datom))))
        outs   (group-by np/src edges)
        ins    (group-by np/dst edges)
        arrows (group-by np/src arcs)]
    (into (i/int-map)
          (concat (nodes db outs ins)
                  (stops db ins arrows)))))

#_(time
    (let [network @(first @(:networks (:router hiposfer.kamal.dev/system)))]
      (last (::foo (assoc network ::foo (create network))))))

(defn osm-node? [o] (instance? PedestrianNode o))

(defn gtfs-stop? [o] (instance? TransitStop o))
