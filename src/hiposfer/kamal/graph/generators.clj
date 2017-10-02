(ns hiposfer.kamal.graph.generators
  (:require [clojure.test.check.generators :as gen]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]
            [hiposfer.kamal.graph.protocols :as rp]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.graph.core :as route]
            [clojure.data.int-map :as imap])) ;; loads the spec in the registry

(defn- grapher
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [pick     #(rand-nth (seq ids))
        lat-gen   (partial gen/generate (s/gen ::geojson/lat))
        lon-gen   (partial gen/generate (s/gen ::geojson/lon))
        arcer    #(route/->Edge (pick) (pick) (rand-int (* 3 (count ids))))
        ;; create 3 times as many edges as node IDs
        edges     (repeatedly (* 3 (count ids)) arcer)
        ;; first create nodes to store the edges
        graph     (into (imap/int-map) (map #(vector % (route/->NodeInfo (lat-gen)
                                                                         (lon-gen)
                                                                         nil nil)))
                        ids)]
    ;; now connect the nodes
    (reduce rp/connect graph edges)))

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

(defn complete
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
        ways    (map (fn [id] [id {:name (gen/generate namer)}])
                     way-ids)]
    {:graph graph
     :ways  (into {} ways)}))

;; example usage
;(complete (gen/generate (graph 10)))