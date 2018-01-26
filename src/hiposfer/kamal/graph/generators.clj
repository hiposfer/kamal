(ns hiposfer.kamal.graph.generators
  (:require [clojure.test.check.generators :as gen]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]
            [hiposfer.kamal.graph.protocols :as rp]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.graph.core :as graph]
            [clojure.data.int-map :as imap]))

(defn- grapher
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [pick     #(rand-nth (seq ids))
        lat-gen   (partial gen/generate (s/gen ::geojson/lat))
        lon-gen   (partial gen/generate (s/gen ::geojson/lon))
        arcer    #(graph/->Edge (pick) (pick) (rand-int (* 3 (count ids))))
        ;; create 3 times as many edges as node IDs
        edges     (distinct (repeatedly (* 3 (count ids)) arcer))
        ;; first create nodes to store the edges
        nodes     (into (imap/int-map) (map #(vector % (graph/->NodeInfo (lat-gen)
                                                                         (lon-gen)
                                                                         nil nil)))
                        ids)]
    (reduce graph/connect nodes edges)))     ;; now connect the nodes

(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap grapher (gen/set (gen/resize (* 3 size) gen/nat)
                             {:min-elements size})))

;;example usage
;(gen/generate (graph 100))

(def string-alpha
  "Generate alpha strings"
  (gen/fmap str/join (gen/vector gen/char-alpha)))

(defn with-ways
  "returns a network with a fake ways element to conform to the generated graph"
  [graph]
  (let [arcs    (map rp/successors (vals graph))
        way-ids (into #{} (comp cat
                                (map rp/way)
                                (remove nil?))
                          arcs)
        ;; An string 90% of the time, nil 10%
        namer   (gen/frequency [[1 (gen/return nil)]
                                [9 (gen/fmap str/capitalize string-alpha)]])
        ways    (map (fn [id] [id {:name (gen/generate namer)
                                   :hiposfer.kamal.osm/nodes (into [] (comp (filter #(= id (rp/way %)))
                                                                            (map rp/src))
                                                                      (graph/edges graph))}])
                     way-ids)]
    {:graph      graph
     :ways       (into {} ways)}))

;; example usage
;(complete (gen/generate (graph 10)))
