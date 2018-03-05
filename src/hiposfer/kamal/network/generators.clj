(ns hiposfer.kamal.network.generators
  (:require [clojure.test.check.generators :as gen]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(defn- grapher
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [pick     #(rand-nth (seq ids))
        lat-gen   (partial gen/generate (s/gen ::geojson/lat))
        lon-gen   (partial gen/generate (s/gen ::geojson/lon))
        arcer    #(hash-map :node/id (pick) :node/neighbours #{(pick)})
        ;; create 3 times as many edges as node IDs
        edges     (distinct (repeatedly (* 3 (count ids)) arcer))
        ;; first create nodes to store the edges
        nodes     (for [id ids]
                    {:node/id id :node/lat (lat-gen) :node/lon (lon-gen)})]
    (concat nodes edges)))     ;; now connect the nodes

(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap grapher (gen/set (gen/resize (* 3 size) gen/nat)
                             {:min-elements size})))
;; example usage
;(gen/generate (graph 100))

(def string-alpha
  "Generate alpha strings"
  (gen/fmap str/join (gen/vector gen/char-alpha)))

(defn- wayver
  "returns a network with a fake ways element to conform to the generated graph"
  [ids]
  (let [;; An string 90% of the time, nil 10%
        namer (gen/frequency [[1 (gen/return nil)]
                              [9 (gen/fmap str/capitalize string-alpha)]])]
    (for [id ids]
      {:way/id id
       :way/name (gen/generate namer)})))

(defn ways
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap wayver (gen/set (gen/resize (* 3 size) gen/nat)
                            {:min-elements size})))
;; example usage
(gen/generate (ways 10))
