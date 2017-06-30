(ns service.prop-test
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            ;[clojure.test.check :as tc]
            [service.routing.algorithms :as alg]
            [service.generators :as g]
            [clojure.test.check.generators :as gen]))

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
(defspec deterministic
  100; tries
  (prop/for-all [graph (gen/such-that not-empty (g/graph 10) 1000)]
    (let [src  (rand-nth (keys graph))
          dst  (rand-nth (keys graph))
          coll (alg/dijkstra graph :start-from #{src}
                              :direction ::alg/forward
                              :value-by alg/length)]
      (apply = (repeatedly 10 #(reduce (fn [_ v] (when (= dst (key v)) (reduced (key v))))
                                       nil
                                       coll))))))

;(tc/quick-check 100 deterministic)
