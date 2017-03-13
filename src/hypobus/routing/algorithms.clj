(ns hypobus.routing.algorithms
  (:require [clojure.data.priority-map :as data]
            [clojure.data.int-map :as imap]
            [hypobus.basics.geometry :as geo]
            [hypobus.routing.osm :as osm]
            [hypobus.routing.core :as route]
            [clojure.set :as set]
            [hypobus.utils.tool :as tool])
  (:import (java.util PriorityQueue Queue)))

(defn- init-queue
  "Returns a new MUTABLE priority queue and adds all the sources id to
  the beginning of the queue."
  ^PriorityQueue
  [init-set]
  (let [cheapest-path (fn [[_ tr1] [_ tr2]] (compare (:cost tr1) (:cost tr2)))]
    (reduce (fn [^PriorityQueue q id] (.add q [id (route/->Trace 0 0 nil)]) q)
            (new PriorityQueue cheapest-path)
            init-set)))

(defn- fast-forward!
  "moves the queue's head to an unsettled node"
  [^PriorityQueue queue settled]
  (while (and (not (.isEmpty queue)) (contains? settled (first (.peek queue))))
    (.poll queue))) ;; ignore settled nodes

(defn- step!
  "utility function. Meant to be used in both dijkstra and bidirectional
  dijkstra. It mutates the queue adding the arcs arcs to it"
  [graph arcs router settled ^PriorityQueue queue curr-id trz]
  (let [relax (fn [_ dst arc]
                (let [[cost span] (route/worth router arc settled)]
                  (.add queue [dst (route/->Trace (+ cost (:cost trz))
                                                  (+ span (:time trz))
                                                  curr-id)])))]
    (reduce-kv relax queue (arcs (get graph curr-id))))) ;; add new neighbors

(defn dijkstra-1d
  "computes the shortest path between a set of sources and any other node in graph
  using Dijkstra's algorithm with a priority queue. Stops whenever the router
  fulfills its matching criteria or when no more nodes can be reached.
  init-set is a set of node ids to populate the queue at the beginning.
  arcs is either :out-arcs or :in-arcs.
  Returns a vector of [success? settled-nodes]"
  [graph router init-set arcs]
  (let [queue (init-queue init-set)]
    (loop [settled (imap/int-map)
           halt    false]
      (fast-forward! queue settled) ;; ignore settled nodes
      (if (or halt (.isEmpty queue)) [halt settled]
        (let [[curr-id trz] (.poll queue)
              _             (step! graph arcs router settled queue curr-id trz)
              resettled     (persistent! (assoc! (transient settled) curr-id trz))]
          (recur resettled (route/stop? router resettled curr-id)))))))

(defn dijkstra-2d
  "computes the shortest path between a set of sources and any other node in graph
  using a Bidirectional (2d) Dijkstra's algorithm with a priority queue. Stops
  whenever the router fulfills its matching criteria or when no more nodes can
  be reached. Returns a vector of
  [success? settled-forward unsettled-forward settled-backward unsettled-backward]"
  [graph router]
  (let [vanguard  (init-queue (route/sources router))
        rearguard (init-queue (route/destinations router))]
    (loop [forward  (imap/int-map)
           backward (imap/int-map)
           halt     false]
      (fast-forward! vanguard forward) ;; ignore settled nodes
      (fast-forward! rearguard backward) ;; ignore settled nodes
      (if-not (or halt (.isEmpty vanguard) (.isEmpty rearguard))
        (let [[fid f-trz] (.poll vanguard) ;; forward
              [bid b-trz] (.poll rearguard) ;; backward
              _           (step! graph :out-arcs router forward vanguard fid f-trz)
              _           (step! graph :in-arcs router backward rearguard bid b-trz)
              refor       (persistent! (assoc! (transient forward) fid f-trz))
              reback      (persistent! (assoc! (transient backward) bid b-trz))]
          (recur refor reback (route/stop? router forward fid backward bid)))
        [halt
         forward
         (into (imap/int-map)
               (comp (tool/unique-by first) (remove #(contains? forward (first %))))
               vanguard)
         backward
         (into (imap/int-map)
               (comp (tool/unique-by first) (remove #(contains? backward (first %))))
               rearguard)]))))

;; TODO: this doesnt have a deterministic behavior. It returns different things with
;; the same input. No idea why
#_(defn dijkstra-2dp
    "computes the shortest path between a set of sources and any other node in graph
    using a multi-thread Bidirectional (2d) Dijkstra's algorithm with a priority queue.
    Stops whenever the router fulfills its matching criteria or when no more nodes can
    be reached. Returns a vector of
    [success? settled-forward unsettled-forward settled-backward unsettled-backward]"
    [graph router]
    (let [vanguard  (volatile! (init-queue (route/sources router)))
          rearguard (volatile! (init-queue (route/destinations router)))
          forward   (volatile! (imap/int-map))
          backward  (volatile! (imap/int-map))
          fvid      (volatile! 0)
          bvid      (volatile! 0)
          halt      (atom false)]
      (future
        (while (not (or @halt (.isEmpty ^Queue @vanguard) (.isEmpty ^Queue @rearguard)))
          (fast-forward! @vanguard @forward) ;; ignore settled nodes
          (let [[fid f-trz] (.poll ^Queue @vanguard) ;; forward
                _           (vreset! fvid fid)
                _           (step! graph :out-arcs router @forward @vanguard fid f-trz)
                refor       (vswap! forward assoc fid f-trz)]
            (reset! halt (route/stop? router @forward fid @backward @bvid)))))
      (while (not (or @halt (.isEmpty ^Queue @vanguard) (.isEmpty ^Queue @rearguard)))
        (fast-forward! @rearguard @backward) ;; ignore settled nodes
        (let [[bid b-trz] (.poll ^Queue @rearguard) ;; backward
              _           (vreset! bvid bid)
              _           (step! graph :in-arcs router @backward @rearguard bid b-trz)
              reback      (vswap! backward assoc bid b-trz)]
            (reset! halt (route/stop? router @forward @fvid @backward bid))))
      [@halt
       @forward
       (into (imap/int-map)
             (comp (tool/unique-by first) (remove #(contains? @forward (first %))))
             (.toArray ^PriorityQueue @vanguard))
       @backward
       (into (imap/int-map)
             (comp (tool/unique-by first) (remove #(contains? @backward (first %))))
             (.toArray ^PriorityQueue @rearguard))]))

(defn dijkstra
  [graph router]
  (condp = (route/direction router)
    ::route/forward (dijkstra-1d graph router (route/sources router) :out-arcs)
    ::route/backward (dijkstra-1d graph router (route/destinations router) :in-arcs)
    ::route/bidirectional (dijkstra-2d graph router)
    ;::route/parallel (dijkstra-2dp graph router)
    (ex-info (str "unknown direction " (route/direction router) " in ") router)))


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
  (reduce-kv (fn [best _ node] (min-key #(geo/euclidean-pow2 point %) node best))
             (second (first graph)) graph))

(defn pbrute-nearest
  "search the nearest node in graph to point using the euclidean function
  WARNING: This function does NOT perform well on small graphs probably due to
  the euclidean pow2 not being a computationally intensive function"
  [graph point]
  (println (:lat point) (:lon point))
  (let [geo-dist #(geo/euclidean-pow2 point %)
        reducef   (fn [best _ node] (min-key geo-dist node best))
        combinef (fn ([] (second (first graph)))
                     ([v] (apply min-key geo-dist v)))]
    (clojure.core.reducers/fold combinef reducef graph)))

(let [point (rand-nth (vals @osm/foo))]
  (time (brute-nearest @osm/foo point))
  (time (pbrute-nearest @osm/foo point)))

;;;;;;;;;;;; PLAYGROUND ;;;;;;;;;;;;;;;;;;;;
(defn- random-path
  [graph]
  (let [src    (rand-nth (keys graph))
        dst    (rand-nth (keys graph))
        router (route/->ArcLengthRouter src dst ::route/parallel)
        start  (System/currentTimeMillis)
        result (dijkstra graph router)
        end    (System/currentTimeMillis)]
    (- end start)))

(time (let [res (map #(random-path %) (repeat 50 @osm/foo))]
        (double (/ (reduce + res) 50))))

;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/forward)))
;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/backward)))
;(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/bidirectional)))
(time (dijkstra @osm/foo (route/->ArcLengthRouter 524154095 17029369 ::route/parallel)))
;; stops at 258742 settled size

(def rosetta {1 {:out-arcs {2 {:dst 2 :length 7 :kind :other}
                            3 {:dst 3 :length 9 :kind :other}
                            6 {:dst 6 :length 14 :kind :other}}
                 :in-arcs  {}}
              2 {:out-arcs {3 {:dst 3 :length 10 :kind :other}
                            4 {:dst 4 :length 15 :kind :other}}
                 :in-arcs  {1 {:src 1 :length 7 :kind :other}}}
              3 {:out-arcs {4 {:dst 4 :length 11 :kind :other}
                            6 {:dst 6 :length 2 :kind :other}}
                 :in-arcs  {1 {:src 1 :length 9 :kind :other}
                            2 {:src 2 :length 10 :kind :other}}}
              4 {:out-arcs {5 {:dst 5 :length 6 :kind :other}}
                 :in-arcs  {2 {:src 2 :length 15 :kind :other}
                            3 {:src 3 :length 11 :kind :other}}}
              5 {:out-arcs {6 {:dst 6 :length 9 :kind :other}}
                 :in-arcs  {4 {:src 4 :length 6 :kind :other}}}
              6 {:out-arcs {}
                 :in-arcs  {1 {:src 1 :length 14 :kind :other}
                            3 {:src 3 :length 2 :kind :other}
                            5 {:src 5 :length 9 :kind :other}}}})

;(time (path rosetta (dijkstra rosetta 1 5) 5))
(dijkstra rosetta (route/->ArcLengthRouter 1 5 ::route/bidirectional))
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

; (hypobus.utils.mapbox/write-geojson "resources/route.json"
;   (sequence (comp (map #(get @osm/foo %))
;                   (map #(select-keys % [:lon :lat])))
;             foo))
