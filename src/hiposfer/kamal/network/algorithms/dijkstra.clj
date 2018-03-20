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
    (run! (fn [value] (let [[e v] (if (vector? value) value [value 0])
                            t (trace e nil)
                            he (.insert queue v t)]
                        (.put settled value he)))
          init-set)
    queue))

(defn- path
  "returns a lazy sequence of immutable map entries starting at from and
  going back until no previous entry is found"
  [^Map settled from]
  (let [entry ^Heap$Entry (.get settled from)
        prev-id           (val (.getValue entry))]
    (cons (trace from (.getKey entry))
          (lazy-seq (when prev-id (path settled prev-id))))))

(defn- relax!
  "calculate the weight of traversing arc and updates it if already in
  the queue or adds it otherwise"
  [value ^Map settled ^Map unsettled ^Heap$Entry entry ^Heap queue trail node-successors]
  (if (empty? node-successors) nil ;; nothing more to process
    (if (.containsKey settled (first node-successors)) ;; node already settled, continue
      (recur value settled unsettled entry queue trail (rest node-successors))
      (let [entity  (first node-successors) ;; nothing settled, compute value
            v       (value entity trail)]
        (if (nil? v) ;; no path, infinite cost -> ignore it
          (recur value settled unsettled entry queue trail (rest node-successors))
          (let [prev    (key (.getValue entry))
                weight  (np/sum v (.getKey entry))
                trace2  (trace entity prev)
                old-entry ^Heap$Entry (.get unsettled entity)]
            (if (nil? old-entry) ;; new entry
              (.put unsettled entity (.insert queue weight trace2))
              (when (< (np/cost weight) (np/cost (.getKey old-entry)))
                (.setValue old-entry trace2) ;; new entry has better cost
                (.decreaseKey queue old-entry weight)))
            (recur value settled unsettled entry queue ;; continue to next node
                   trail (rest node-successors))))))))

(defn- produce!
  "returns a lazy sequence of traces by sequentially mutating the
   queue and always returning the path from the latest min priority
   node"
  [graph value successors ^Heap queue ^Map settled ^Map unsettled]
  (let [entry  (.extractMinimum queue)
        entity (key (.getValue entry))
        _      (.put settled entity entry)
        _      (.remove unsettled entity)
        trail  (path settled entity)]
    (relax! value settled unsettled entry queue
            trail (successors graph entity))
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
;
; the elements necessary to initialize a Dijkstra collection are
; - graph: an collection of nodes over which the traversal will happen
; - ids: a #{ids}
; - value: a function of next-node, current-trace -> Valuable implementation
; - successors: a function of id -> [ id ]. Used to get either the outgoing
;               arcs of a node
(deftype Dijkstra [graph start-from value successors comparator]
  Seqable
  (seq [_]
    (let [settled   (new HashMap) ;{id {weight {id prev}}}
          queue     (init! start-from settled comparator) ;[{weight {id prev}}]
          unsettled (new HashMap); {id {weight {id prev}}}
          trailer!  (fn trailer! []
                      (if (.isEmpty queue) (list)
                        (cons (produce! graph value successors queue settled unsettled)
                              (lazy-seq (trailer!)))))]
      (trailer!)))
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
             trail (produce! graph value successors queue settled unsettled)]
        (let [rr (rf ret trail)]
          (if (reduced? rr) @rr
            (if (.isEmpty queue) rr
              (recur rr (produce! graph value successors queue settled unsettled))))))))
  ;; ------
  IReduce
  (reduce [this rf] (.reduce ^IReduceInit this rf (rf)))
  ;; declaring as Sequential will cause the seq to be used for nth, etc
  Sequential)

