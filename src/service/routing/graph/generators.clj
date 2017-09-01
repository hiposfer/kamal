(ns service.routing.graph.generators
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [service.routing.graph.specs]
            [clojure.string :as str]
            [service.routing.graph.protocols :as rp])) ;; loads the spec in the registry

(def doubler (gen/double* {:infinite? false
                           :NaN? false}))
                           ;:min       - minimum value (inclusive, default none)
                           ;:max       - maximum value (inclusive, default none)}))

(defn- grapher
  "returns a graph based on the provided node ids. Random latitude and longitudes
  are generated as well"
  [ids]
  (let [pick     #(rand-nth (seq ids))
        arcer    #(hash-map :src (pick) :dst (pick)
                            :way (rand-int (* 3 (count ids))))
        arcs      (repeatedly (* 3 (count ids)) arcer)
        outgoings (group-by rp/src arcs)
        incomings (group-by rp/src (map rp/mirror arcs))
        ;; first assoc all outgoing arcs
        graph     (reduce-kv (fn [res node-id arcs] (assoc-in res [node-id :arcs]
                                                      (into {} (map #(vector (rp/dst %) %) arcs))))
                             {}
                             outgoings)
        ;; now mirror the outgoing arcs to make it a bidirectional graph
        graph2    (reduce-kv (fn [res node-id arcs] (update-in res [node-id :arcs] merge
                                                      (into {} (map #(vector (rp/dst %) %) arcs))))
                             graph
                             incomings)]
    ;; now create random lat, lon pairs
    (reduce-kv (fn [res id _] (update res id merge {:lat (gen/generate doubler)
                                                    :lon (gen/generate doubler)}))
               graph2
               graph2)))

(defn graph
  "returns a graph generator with node's id between 0 and 3*size.
  The generator creates a minimum of size elements"
  [size]
  (gen/fmap grapher (gen/set (gen/resize (* 3 size) gen/nat))))

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
        namer   (gen/fmap str/capitalize string-alpha)
        ways    (map (fn [id] [id {:name (gen/generate (gen/such-that not-empty namer))}])
                     way-ids)]
    {:graph graph
     :ways  (into {} ways)}))

;; example usage
;(complete (gen/generate (graph 10)))
