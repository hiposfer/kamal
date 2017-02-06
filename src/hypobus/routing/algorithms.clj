(ns hypobus.routing.algorithms
  (:require [clojure.data.priority-map :as data]
            [clojure.data.int-map :as imap]
            [hypobus.routing.osm :as osm]
            [hypobus.routing.core :as route])
  (:import (java.util PriorityQueue)))

(defn- fast-forward!
  "moves the queue's head to an unsettled node"
  [^PriorityQueue queue settled]
  (while (and (not (.isEmpty queue)) (contains? settled (first (.peek queue))))
    (.poll queue))) ;; ignore settled nodes

(defn dijkstra
  "computes the shortest path between src-id and any other node in graph
  using Dijkstra's algorithm with a priority queue. Stops whenever the router
  fulfills its matching criteria or when no more nodes can be reached.
  Returns whatever the router decides to."
  [graph src-id router]
  (loop [settled (imap/int-map)
         queue   (doto (new PriorityQueue (fn [[_ tr1] [_ tr2]]
                                            (compare (:cost tr1) (:cost tr2))))
                       (.add [src-id (route/->Trace 0 nil)]))]
    (fast-forward! queue settled) ;; ignore settled nodes
    (if (or (route/stop? router settled) (.isEmpty queue))
      (route/return router settled)
      (let [[curr-id trz] (.poll queue)
            relax         (fn [_ dst arc]
                            (if (contains? settled dst) queue
                              (let [alt-cost (+ (route/cost router arc settled)
                                                (:cost trz))]
                                (.add queue [dst (route/->Trace alt-cost curr-id)]))))]
           ;; add new neighbors
           (reduce-kv relax queue (:out-arcs (get graph curr-id)))
           (recur (persistent! (assoc! (transient settled) curr-id trz))
                  queue)))))


(defn path
  "uses the traces structure to reconstruct the path taken to reach the
  destination id"
  [graph traces dst-id]
  (let [reids (iterate #(:previous (get traces %)) dst-id)
        path  (reverse (take-while identity reids))]
    path))


;;;;;;;;;;;; PLAYGROUND ;;;;;;;;;;;;;;;;;;;;
(defn- random-path
  [graph]
  (let [src    (rand-nth (keys graph))
        router (route/->SimpleDstRouter (rand-nth (keys graph)))
        start  (System/currentTimeMillis)
        traces (dijkstra graph src router)
        end    (System/currentTimeMillis)]
    (- end start)))

(time (let [res (map #(random-path %) (repeat 100 @osm/foo))]
           (double (/ (reduce + res) 100))))

;(random-path @osm/foo)

;(def rosetta {1 {:out-arcs {2 {:dst-id 2 :length 7  :kind :other}
;                            3 {:dst-id 3 :length 9  :kind :other}
;                            6 {:dst-id 6 :length 14 :kind :other}}}
;              2 {:out-arcs {3 {:dst-id 3 :length 10 :kind :other}
;                            4 {:dst-id 4 :length 15 :kind :other}}}
;              3 {:out-arcs {4 {:dst-id 4 :length 11 :kind :other}
;                            6 {:dst-id 6 :length 2  :kind :other}}}
;              4 {:out-arcs {5 {:dst-id 5 :length 6  :kind :other}}}
;              5 {:out-arcs {6 {:dst-id 6 :length 9  :kind :other}}}
;              6 {:out-arcs {}}})

;(time (path rosetta (dijkstra rosetta 1 5) 5))
;(dijkstra rosetta 1 (route/->SimpleDstRouter 5))
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

; (hypobus.utils.mapbox/write-geojson "resources/route.json"
;   (sequence (comp (map #(get @osm/foo %))
;                   (map #(select-keys % [:lon :lat])))
;             foo))

