(ns backend.routing.algorithms                              ;; TODO give a proper name
  (:require [backend.utils.tool :as utils]
            [backend.routing.core :as route]
            [clojure.data.int-map :as imap])
  (:import (clojure.lang IPersistentMap)))

(defn path
  "uses the traces structure to reconstruct the path taken to reach the
  destination id"
  [graph traces dst-id]
  (let [reids (iterate #(:previous (get traces %)) dst-id)
        ids   (reverse (take-while identity reids))]
    (map #(get graph %) ids)))

(defn brute-nearest
  "search the nearest node in graph to point using the euclidean function"
  [graph point]
  (println (:lat point) (:lon point))
  (reduce-kv (fn [best _ node] (min-key #(utils/euclidean-pow2 point %) node best))
             (second (first graph)) graph))

;(def foo (delay (time (osm/cleanup (osm/osm->graph "resources/osm/saarland.osm")))))

;(let [point (rand-nth (vals @osm/foo))]
;  (time (brute-nearest @osm/foo point))
;  (time (pbrute-nearest @osm/foo point)))

;;;;;;;;;;;; PLAYGROUND ;;;;;;;;;;;;;;;;;;;;
;(defn- random-path
;  [graph]
;  (let [src    (rand-nth (keys graph))
;        dst    (rand-nth (keys graph))
;        router (route/->ArcLengthRouter src dst ::route/parallel)
;        start  (System/currentTimeMillis)
;        result (dijkstra graph router)
;        end    (System/currentTimeMillis)]
;    (- end start)))

;(time (let [res (map #(random-path %) (repeat 50 @osm/foo))]
;        (double (/ (reduce + res) 50))))

;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/forward)))
;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/backward)))
;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/bidirectional)))
;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/parallel)))
;; stops at 258742 settled size

;; NOTE: for testing purposes only
(def rosetta {1 {:out-arcs {2 {:dst 2 :length 7  :kind :other}
                            3 {:dst 3 :length 9  :kind :other}
                            6 {:dst 6 :length 14 :kind :other}}
                 :in-arcs  {}}
              2 {:out-arcs {3 {:dst 3 :length 10 :kind :other}
                            4 {:dst 4 :length 15 :kind :other}}
                 :in-arcs  {1 {:src 1 :length 7  :kind :other}}}
              3 {:out-arcs {4 {:dst 4 :length 11 :kind :other}
                            6 {:dst 6 :length 2  :kind :other}}
                 :in-arcs  {1 {:src 1 :length 9  :kind :other}
                            2 {:src 2 :length 10 :kind :other}}}
              4 {:out-arcs {5 {:dst 5 :length 6  :kind :other}}
                 :in-arcs  {2 {:src 2 :length 15 :kind :other}
                            3 {:src 3 :length 11 :kind :other}}}
              5 {:out-arcs {6 {:dst 6 :length 9  :kind :other}}
                 :in-arcs  {4 {:src 4 :length 6  :kind :other}}}
              6 {:out-arcs {}
                 :in-arcs  {1 {:src 1 :length 14 :kind :other}
                            3 {:src 3 :length 2  :kind :other}
                            5 {:src 5 :length 9  :kind :other}}}})

;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

(defn length
  [arc _]
  (let [val (/ (:length arc) (get (:kind arc) route/speeds route/min-speed))]
    (route/->SimpleValue val)))

(extend-protocol route/Context
  IPersistentMap
  (predecessors [this] (:in-arcs this))
  (successors    [this] (:out-arcs this)))

;(dijkstra rosetta (route/->ArcLengthRouter 1 5 ::route/forward))

;(def performer (route/dijkstra rosetta :worth length
;                                       :direction ::route/forward
;                                       :src   #{1}))
;; ------- examples
;(reduce
;  (fn [res v]
;    (if-not (nil? res)
;      (println (map route/id (route/path res))
;               (map route/id (route/path v)))
;      (println res (map route/id (route/path v)))))
;  performer)
;
;(reduce
;  (fn [res v] (println res (map route/id (route/path v))))
;  nil
;  performer)
;
;
;(map (comp (partial map route/id) route/path) performer)
;(map route/id (route/path (nth performer 6)))

;(transduce (halt-when (fn bar [v] (= 4 (:id v))))
;           (fn foo [_ v] (println "id" (:id v)))
;           nil
;           performer)