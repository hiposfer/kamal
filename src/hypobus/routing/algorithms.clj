(ns hypobus.routing.algorithms
  (:require [clojure.data.priority-map :as data]
            [clojure.data.int-map :as imap]
            [hypobus.routing.osm :as osm]
            [hypobus.routing.core :as route])
  (:import (java.util PriorityQueue)))

;; TODO: router/cost should have access to the previously computed paths not just
;; to the queue
(defn- relax-arc
  "Compares the current cost to reach dst against reaching it over
  edge (alternative). Return [dst cost] when the alternative is cheaper
  otherwise nil"
  [router queue curr-id dst edge]
  (let [alternative (+ (:cost (get queue curr-id))
                       (route/cost router edge queue)) ;; from-to cost
        current     (-> (get queue dst)
                        (get :cost Double/POSITIVE_INFINITY))] ;; current cost
    (when (> current alternative)
      [dst (route/->Trace alternative curr-id)])))

;; NOTE: prefer zero? count over empty?. See priority-map src code.
;;       This was found out using VisualVm CPU profiler
;; TODO: try a java PriorityQueue for better performance
;; TODO: ------ PROFILE ----------------
;; TODO: check https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentQueue.java
(defn dijkstra
  "computes the shortest path between src-id and any other node in graph
  using Dijkstra's algorithm with a priority queue. Stops whenever the router
  fulfills its matching criteria or when no more nodes can be reached.
  Returns whatever the router decides to."
  [graph src-id router]
  (loop [traces (transient (imap/int-map src-id (route/->Trace 0 nil)))
         queue  (data/priority-map-keyfn :cost src-id (route/->Trace 0 nil))]
    (if (or (route/stop? router traces) (zero? (count queue)))
      (route/return router (persistent! traces))
      (let [[curr-id trz] (peek queue)
            arcs          (:out-arcs (get graph curr-id))
            settler       (fn [q k v]
                            (if (contains? traces k) q
                              (let [settled (relax-arc router q curr-id k v)]
                                (if (nil? settled) q
                                  (conj q settled)))))]
          (recur (assoc! traces curr-id trz)
                 (pop (reduce-kv settler queue arcs)))))))

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

(time (let [res (map #(random-path %) (repeat 50 @osm/foo))]
           (double (/ (reduce + res) 50))))

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
