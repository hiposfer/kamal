(ns hiposfer.kamal.router.generators
  (:require [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [datascript.core :as data]
            [hiposfer.geojson.specs :as geojson]
            [hiposfer.kamal.router.algorithms.core :as network]
            [hiposfer.kamal.router.core :as router]))

(def string-alpha
  "Generate alpha strings"
  (gen/fmap str/join (gen/vector gen/char-alpha 10)))

(defn fake-ways
  "returns a sequence of way objects ready to be used in a Datascript transaction"
  [ids node-ids]
  ;; An string 90% of the time, nil 10%
  (let [namer (gen/frequency [[1 (gen/return nil)]
                              [9 (gen/fmap str/capitalize string-alpha)]])]
    (for [id ids
          :let [name (gen/generate namer)]]
      (merge {:way/id id
              :way/nodes (random-sample 0.3 node-ids)}
             (when (some? name)
               {:way/name name})))))

(defn- fake-nodes
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [lat-gen (partial gen/generate (s/gen ::geojson/lat))
        lon-gen (partial gen/generate (s/gen ::geojson/lon))]
    (for [id ids]
      {:node/id id
       :node/location (network/->Location (lon-gen) (lat-gen))})))

(defn osm-gen
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  ;; we dont support fake GTFS data for development yet
  (let [db    (data/empty-db router/schema)
        nodes (fake-nodes (gen/generate (gen/set (gen/resize (* 3 size) gen/nat)
                                                 {:min-elements size})))
        ways  (fake-ways (gen/generate (gen/set (gen/resize size gen/nat)
                                                {:min-elements (/ size 3)}))
                         (set (map :node/id nodes)))]
    (data/db-with db (concat nodes ways))))

;; example usage
;(graph 100)

(defn osm
  "builds a datascript in-memory db and returns it. Only valid for
  pedestrian routing"
  ([]
   (osm 100))
  ([size]
   (gen/fmap osm-gen (gen/resize (* 3 size) gen/nat))))

;; example usage
;(gen/generate (pedestrian-graph 3))
