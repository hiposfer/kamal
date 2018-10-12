(ns hiposfer.kamal.network.generators
  (:require [clojure.test.check.generators :as gen]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.network.core :as network]
            [datascript.core :as data]
            [hiposfer.kamal.network.algorithms.core :as alg]
            [hiposfer.kamal.services.routing.core :as router]))

(def string-alpha
  "Generate alpha strings"
  (gen/fmap str/join (gen/vector gen/char-alpha)))

(defn fake-ways
  "returns a sequence of way objects ready to be used in a Datascript transaction"
  [ids]
  ;; An string 90% of the time, nil 10%
  (let [namer (gen/frequency [[1 (gen/return "")]
                              [9 (gen/fmap str/capitalize string-alpha)]])]
    (for [id ids]
      {:way/id id
       :way/name (gen/generate namer)})))

(defn- fake-edges
  [node-ids way-ids]
  (let [node-id  #(rand-nth (seq node-ids))
        way-id   #(rand-nth (seq way-ids))
        arcer    #(let [src (node-id)
                        dst (node-id)
                        way (way-id)]
                    (if (not= src dst)
                      {:node/id   src
                       :node/arcs #{{:arc/dst [:node/id dst]
                                     :arc/way [:way/id way]}}}
                      (recur)))]
    ;; create 3 times as many edges as node IDs
    (repeatedly (* 3 (count node-ids)) arcer)))

(defn- fake-nodes
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [lat-gen (partial gen/generate (s/gen ::geojson/lat))
        lon-gen (partial gen/generate (s/gen ::geojson/lon))]
    (for [id ids]
      {:node/id id
       :node/location (network/->Location (lon-gen) (lat-gen))})))

;; TODO: this is far from ideal. Currently we just generate nodes here
;; and then later on we generate ways and HOPE that they match. This
;; can lead to bugs since the generated graph is not consistent
(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  ;; we dont support fake GTFS data for development yet
  (let [db    (data/empty-db router/schema)
        nodes (fake-nodes (gen/generate (gen/set (gen/resize (* 3 size) gen/nat)
                                                 {:min-elements size})))
        ways  (fake-ways (gen/generate (gen/set (gen/resize size gen/nat)
                                                {:min-elements (/ size 3)})))
        edges (fake-edges (set (map :node/id nodes))
                          (set (map :way/id ways)))]
    (data/db-with db (concat nodes ways edges))))

;; example usage
;(graph 100)

(defn pedestrian-graph
  "builds a datascript in-memory db and returns it. Only valid for
  pedestrian routing"
  ([]
   (pedestrian-graph 100))
  ([size]
   (gen/fmap graph (gen/resize (* 3 size) gen/nat))))

;; example usage
;(gen/generate (pedestrian-graph 3))
