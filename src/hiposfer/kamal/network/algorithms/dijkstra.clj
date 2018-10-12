(ns hiposfer.kamal.network.algorithms.dijkstra
  (:require [hiposfer.kamal.network.algorithms.protocols :as np])
  (:import (java.util HashMap Map AbstractMap$SimpleImmutableEntry)
           (clojure.lang Seqable IReduceInit IReduce Sequential)
           (org.teneighty.heap FibonacciHeap Heap Heap$Entry)))


;; ------------------ DIJKSTRA CORE -------------------

(defmacro trace [k v] `(new AbstractMap$SimpleImmutableEntry ~k ~v))

(defn- init!
  "returns a new MUTABLE fibonacci heap (priority queue) and adds all the
   sources id to the beginning of the queue and to the settled map."
  ^Heap [init-set ^Map settled comparator]
  (let [queue  ^Heap (new FibonacciHeap comparator)]
    (doseq [entry init-set]
      (let [[id cost]  (if (vector? entry) entry [entry 0])
            pair       (trace id nil)
            heap-entry (.insert queue cost pair)]
        (.put settled entry heap-entry)))
    queue))

(defn- path
  "returns a lazy sequence of immutable map entries starting at from and
  going back until no previous entry is found. The sequence has the shape
  [[id weight] ..."
  [^Map settled from];; settled -> {id {weight {id prev}}}
  (let [entry ^Heap$Entry (.get settled from)
        prev-id           (val (.getValue entry))]
    (cons (trace from (.getKey entry))
          (lazy-seq (when prev-id (path settled prev-id))))))

(defn- relax!
  "returns a lazy sequence of traces by sequentially mutating the
   queue and always returning the path from the latest min priority
   node"
  [router ^Heap queue ^Map settled ^Map unsettled]
  (let [entry  (. queue (extractMinimum))
        entity (key (. entry (getValue)))
        _      (. settled (put entity entry))
        _      (. unsettled (remove entity))
        trail  (path settled entity)]
    (doseq [arc (np/arcs router entity)
            :let [node (np/dst router arc)]
            :when (not (. settled (containsKey node)))
            :let [weight (np/relax router arc trail)]
            :when (some? weight)]
      (let [prev    (key (. entry (getValue)))
            trace2  (trace node prev)
            old-entry ^Heap$Entry (. unsettled (get node))]
        (if (nil? old-entry) ;; new entry
          (. unsettled (put node (. queue (insert weight trace2))))
          (when (< (np/cost weight) (np/cost (. old-entry (getKey))))
            (. old-entry (setValue trace2)) ;; new entry has better cost
            (. queue (decreaseKey old-entry weight))))))
    trail))

; inspired by http://insideclojure.org/2015/01/18/reducible-generators/
; A Collection type which can reduce itself faster than first/next traversal over
; its lazy representation. For convenience a lazy implementation is also provided.
;
; The Dijkstra algorithm implemented here works as follows:
; 1 - take a set of start node, assign them a weight of zero and add them as
;     initialization arguments to a priority queue as trace instances
; 2 - poll the trace with the lowest cost from the priority queue
; 3 - if there are no more traces - STOP
; 4 - otherwise call the reducing function on the trace
; 5 - if the value returned is a reduced flag - STOP
; 6 - otherwise get the outgoing or incoming arcs of the current node and
;     add them to the priority queue
; 6.1 - create a new trace by adding the current trace cost with the delta
;       returned by the value function
; 7 - repeat steps 2 to 6 until a STOP condition is reached
;
; From the previous description it should be clear that this implementation does
; not have a fixed stop condition. Therefore it is (hopefully) very flexible
; regarding is usefulness.
; Some possible uses are:
; - single source - single destination shortest path
; - multi source - single destination shortest path
; - multi source - multi destination shortest path
; - single source - any/all destination shortest path
; - shortest path with timeout
(deftype Dijkstra [router start-from comparator]
  Seqable
  (seq [_]
    (let [settled   (new HashMap) ;{id {weight {id prev}}}
          queue     (init! start-from settled comparator) ;[{weight {id prev}}]
          unsettled (new HashMap)]; {id {weight {id prev}}}
      (for [_ (range) ;; HACK: range is infinite so we use the queue to stop :)
            :while (not (. queue (isEmpty)))]
        (relax! router queue settled unsettled))))
  ;; ------
  ;; this implementation uses mutable internal data structures but exposes only
  ;; immutable data structures.
  ;; Inspired by: http://www.keithschwarz.com/interesting/code/?dir=dijkstra
  IReduceInit
  (reduce [_ rf init]
    ;; Heap.Entry -> {weight {id prev}}
    (let [settled   (new HashMap); {id Heap.Entry}
          queue     (init! start-from settled comparator); [Heap.Entry]
          unsettled (new HashMap)]; {id Heap.Entry}
      (loop [ret init
             trail (relax! router queue settled unsettled)]
        (let [rr (rf ret trail)]
          (if (reduced? rr) @rr
            (if (. queue (isEmpty)) rr
              (recur rr (relax! router queue settled unsettled))))))))
  ;; ------
  IReduce
  (reduce [this rf] (.reduce ^IReduceInit this rf (rf)))
  ;; declaring as Sequential will cause the seq to be used for nth, etc
  Sequential)

