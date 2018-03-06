(ns hiposfer.kamal.network.algorithms.dijkstra
  (:require [hiposfer.kamal.network.algorithms.protocols :as np]
            [datascript.core :as data])
  (:import (java.util HashMap Map AbstractMap$SimpleImmutableEntry)
           (clojure.lang Seqable IReduceInit IReduce Sequential)
           (org.teneighty.heap FibonacciHeap Heap Heap$Entry)))


;; ------------------ DIJKSTRA CORE -------------------

(defmacro trace [k v] `(new AbstractMap$SimpleImmutableEntry ~k ~v))

(defn- init!
  "returns a new MUTABLE fibonacci heap (priority queue) and adds all the
   sources id to the beginning of the queue and to the settled map."
  ^Heap [init-set ^Map settled]
  (let [queue  ^Heap (new FibonacciHeap)]
    (run! (fn [id] (->> (trace id nil)
                        (.insert queue 0)
                        (.put settled id)))
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
  (if (empty? node-successors) nil
    (if (.containsKey settled (first node-successors))
      (recur value settled unsettled entry queue trail (rest node-successors))
      (let [id      (first node-successors)
            prev-id (key (.getValue entry))
            weight  (np/sum (value id trail)
                            (.getKey entry))
            trace2  (trace id prev-id)
            old-entry ^Heap$Entry (.get unsettled id)]
        (if (nil? old-entry)
          (.put unsettled id (.insert queue weight trace2))
          (when (< weight (.getKey old-entry))
            (.setValue old-entry trace2)
            (.decreaseKey queue old-entry weight)))
        (recur value settled unsettled entry queue
               trail (rest node-successors))))))

(defn- produce!
  "returns a lazy sequence of traces by sequentially mutating the
   queue and always returning the path from the latest min priority
   node"
  [graph value successors ^Heap queue ^Map settled ^Map unsettled]
  (let [entry  (.extractMinimum queue)
        id     (key (.getValue entry))
        _      (.put settled id entry)
        _      (.remove unsettled id)
        trail  (path settled id)]
    (relax! value settled unsettled entry queue
            trail (successors (data/entity graph id)))
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
; - network: a {id node} mapping
; - ids: a #{ids}
; - value: a function of current-arc, current-trace -> Valuable implementation
; - arcs: a function of node -> [arc]. Used to get either the incoming or outgoing arcs of a node
; - f: a function of Arc -> id. Used to get the id of the src or dst of an Arc
(deftype Dijkstra [graph ids value successors]
  Seqable
  (seq [_]
    (let [settled   (new HashMap) ;{id {weight {id prev}}}
          queue     (init! ids settled) ;[{weight {id prev}}]
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
          queue     (init! ids settled); [Heap.Entry]
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

