(ns backend.prop-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [backend.routing.algorithms :as alg]
            [backend.routing.core :as core]
            [backend.generators :as g]))

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
;(def deterministic
;  ;300; tries
;  (prop/for-all [graph (g/graph 10)]
;    (let [router (core/->ArcLengthRouter src dst :out-arcs)
;          (apply = (repeatedly (alg/dijkstra-1d graph router (core/sources router) :out-arcs)
;                           10))])))

;(tc/quick-check 100 deterministic)

(def sort-idempotent-prop
  (prop/for-all [v (gen/vector gen/int)]
                (= (sort v) (sort (sort v)))))

(tc/quick-check 100 sort-idempotent-prop)