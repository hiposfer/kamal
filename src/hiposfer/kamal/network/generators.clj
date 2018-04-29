(ns hiposfer.kamal.network.generators
  (:require [clojure.test.check.generators :as gen]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.network.core :as network]))

(defn- grapher
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [pick     #(rand-nth (seq ids))
        lat-gen   (partial gen/generate (s/gen ::geojson/lat))
        lon-gen   (partial gen/generate (s/gen ::geojson/lon))
        arcer    #(let [src (pick)
                        dst (pick)]
                    (if (not= src dst)
                      {:node/id src :node/successors #{[:node/id dst]}}
                      (recur)))
        ;; create 3 times as many edges as node IDs
        edges     (distinct (repeatedly (* 3 (count ids)) arcer))
        ;; first create nodes to store the edges
        nodes     (for [id ids]
                    {:node/id id
                     :node/location (network/->Location (lon-gen) (lat-gen))})]
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

(defn ways
  "returns a sequence of way objects ready to be used in a Datascript transaction"
  [ids]
  (let [;; An string 90% of the time, nil 10%
        namer (gen/frequency [[1 (gen/return "")]
                              [9 (gen/fmap str/capitalize string-alpha)]])]
    (for [id ids]
      {:way/id id
       :way/name (gen/generate namer)
       :way/nodes (for [i (random-sample 0.4 ids)]
                    [:node/id i])})))
