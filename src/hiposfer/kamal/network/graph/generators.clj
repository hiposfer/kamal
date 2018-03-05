(ns hiposfer.kamal.network.graph.generators
  (:require [clojure.test.check.generators :as gen]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.network.graph.core :as graph]))

(defn- grapher
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [pick     #(rand-nth (seq ids))
        lat-gen   (partial gen/generate (s/gen ::geojson/lat))
        lon-gen   (partial gen/generate (s/gen ::geojson/lon))
        arcer    #(hash-map :lat (pick) :lon (pick))
        ;; create 3 times as many edges as node IDs
        edges     (distinct (repeatedly (* 3 (count ids)) arcer))
        ;; first create nodes to store the edges
        nodes     (into {} (map #(vector % (hash-map :lat (lat-gen)
                                                     :lon (lon-gen))))
                           ids)]
    (reduce graph/connect nodes edges)))     ;; now connect the nodes

; TODO: make this a datomized way generator
(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap grapher (gen/set (gen/resize (* 3 size) gen/nat)
                             {:min-elements size})))
;;example usage
;(gen/generate (network 100))

(def string-alpha
  "Generate alpha strings"
  (gen/fmap str/join (gen/vector gen/char-alpha)))

; TODO: make this a datomized way generator
;(defn with-ways
;  "returns a network with a fake ways element to conform to the generated graph"
;  [graph]
;  (let [arcs    (map gp/successors (vals graph))
;        way-ids (into #{} (comp cat
;                                (map np/way)
;                                (remove nil?))
;                          arcs)
;        ;; An string 90% of the time, nil 10%
;        namer   (gen/frequency [[1 (gen/return nil)]
;                                [9 (gen/fmap str/capitalize string-alpha)]])
;        ways    (map (fn [id]
;                       [id {:name (gen/generate namer)
;                            :hiposfer.kamal.osm/nodes
;                                  (into [] (comp (filter #(= id (np/way %)))
;                                                 (map gp/src))
;                                           (graph/edges graph))}])
;                     way-ids)]
;    {:graph      graph
;     :ways       (into {} ways)}))

;; example usage
;(complete (gen/generate (network 10)))
