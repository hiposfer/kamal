(ns backend.prop-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [backend.routing.algorithms :as alg]
            [backend.routing.core :as core]
            [backend.generators :as g]
            [backend.specs :as core.specs])) ;; loads the spec in the registry

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
(def deterministic
  ;300; tries
  (prop/for-all [graph (g/graph 10)
                 src   (gen/elements (keys graph))
                 dst   (gen/elements (keys graph))]
    (let [router (core/->ArcLengthRouter src dst :out-arcs)]
      (apply = (repeatedly (alg/dijkstra-1d graph router (core/sources router) :out-arcs)
                           10)))))

;(tc/quick-check 100 deterministic)