(ns hiposfer.kamal.network.graph.specs
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.network.graph.protocols :as rp]
            [hiposfer.geojson.specs :as geojson]
            [hiposfer.kamal.network.graph.core :as graph]
            [hiposfer.kamal.network.graph.protocols :as gp]))

;;TODO: this has never been tested. It would be nice if you could :)
(s/def ::node (s/and graph/node?
                     #(every? graph/arc? (gp/predecessors %))
                     #(every? graph/arc? (gp/successors %))))
(s/def ::graph (s/every-kv some? ::node))
;; TODO: this doesnt check the relation between nodes, i.e. an edge could
;; have an unexistent node as src/dst ID