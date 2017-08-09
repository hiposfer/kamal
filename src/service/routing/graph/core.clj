(ns service.routing.graph.core
  (:require [service.routing.graph.protocols :as rp]
            [clojure.data.int-map :as imap])
  (:import (java.util Map$Entry Queue PriorityQueue)
           (clojure.lang ILookup IPersistentMap Seqable IReduceInit IReduce Sequential ITransientSet)))

;; ------------------------------------------------------
; graph is an {id node}
; Node is a {:lon :lat :out-arcs {dst-id arc} :in-arcs {src-id arc}}
; Arc is a {:src node-id :dst node-id :length meters :kind OSM-highway-type}
(defrecord Node [^double lon ^double lat out-arcs in-arcs]
  rp/Context
  (predecessors [this] (:in-arcs this))
  (successors   [this] (:out-arcs this)))

(extend-type IPersistentMap
  rp/Context ;; allow Clojure's maps to behave in the same way that Node records
  (predecessors [this] (:in-arcs this))
  (successors    [this] (:out-arcs this))
  rp/Arc
  (src [this] (:src this))
  (dst [this] (:dst this)))

(defrecord Arc  [^long src ^long dst ^double length kind]
  rp/Arc
  (src [_] src)
  (dst [_] dst))

;; a simple representation of a worth function result where both cost and time
;; are the same
(deftype SimpleValue [^double value]
  rp/Valuable
  (cost [_] value)
  (time [_] value)
  (sum  [_ that] (SimpleValue. (+ value (.-value ^SimpleValue that))))
  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [_ k default]
    (case k
      :cost value
      :time value
      default))
  Object
  (toString [_] (str "{:cost " value " :time " value " }")))

; a Trace is MapEntry-like object used to map a node id to a cost without
; using a complex data structure like a hash-map. It is recursive since
; it contains a reference to its previous trace implementation thus only the
; current instance is necessary to determine the complete path traversed up until
; the it.
(deftype IdentifiableTrace [^long id footprint prior]
  rp/Traceable
  (path  [this]
    (if (nil? prior) (list this); todo: should the first element be this? or prior?
                     (cons this (lazy-seq (rp/path prior)))))
  Map$Entry
  (getKey [_] id)
  (getValue [_] footprint) ; we rely on the key and val implementing their own equals
  (setValue [_ _] (throw (ex-info "Unsupported Operation" {} "cannot change an immutable value")))
  (equals [_ that] (and (= id (key that)) (= footprint (val that))))
  (hashCode [_] (hash [id footprint prior]))
  Object
  (toString [_] (str "[" id " " footprint " ]")))

; travis-ci seems to complaint about not finding a matching constructor if the
; init size is not there. Funnily the ctor with a single comparator is not defined
; in the java docs .... hmmm :/
(defn- init-queue
  "Returns a new MUTABLE priority queue and adds all the sources id to
  the beginning of the queue."
  ^Queue
  [init-set]
  (let [cheapest-path (fn ^long [trace1 trace2] (compare (rp/cost (val trace1))
                                                         (rp/cost (val trace2))))
        queue  ^Queue (new PriorityQueue 10 cheapest-path)]; 10 init size
    (run! (fn [id] (.add queue (->IdentifiableTrace id (->SimpleValue 0) nil))) init-set)
    queue))

(defn- poll-unsettled!
  "moves the queue's head to an unsettled node id and returns the element
  containing it

  utility function: DO NOT USE DIRECTLY."
  [^Queue queue ^ITransientSet settled]
  (let [trace (.poll queue)]
    (if (nil? trace) nil
      (if (.contains settled (key trace))
        (recur queue settled)
        trace))))

(defn- relax-nodes!
  "polls the next unsettled trace from the queue and adds all its neighbours
  to it

  utility function: DO NOT USE DIRECTLY."
  [value f node-arcs trace ^Queue queue]
  (reduce (fn [_ arc]
            (let [weight (rp/sum (value arc trace)
                                 (val trace))]
              (.add queue (->IdentifiableTrace (f arc) weight trace))
              queue))
          queue
          node-arcs))

(defn- produce!
  "returns a lazy sequence of traces by sequentially mutating the
  queue (step!(ing) into it) and concatenating the latest poll with
  the rest of them

  utility function: DO NOT USE DIRECTLY"
  [graph value arcs f ^Queue queue settled]
  (let [trace (poll-unsettled! queue settled)]; (step! graph settled value arcs queue)
    (if (nil? trace) (list)
      (let [next-queue   (relax-nodes! value f (arcs (get graph (key trace))) trace queue)
            next-settled (conj! settled (key trace))]
        (cons trace
              (lazy-seq (produce! graph value arcs f next-queue next-settled)))))))


; inspired by http://insideclojure.org/2015/01/18/reducible-generators/
; A Collection type which can reduce itself faster than first/next traversal over its lazy
; representation. For convenience a lazy implementation is also provided.
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
; 6.1 - create a new trace with by adding the current trace cost with the delta
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
; - graph: a {id node} mapping
; - ids: a #{ids}
; - value: a function of current-arc, current-trace -> Valuable implementation
; - arcs: a function of graph, id -> node. Used to get either the incoming or outgoing arcs of a node
; - f: a function of Arc -> id. Used to get the id of the src or dst of an Arc
(deftype Dijkstra [graph ids value arcs f]
  Seqable
  (seq [_]
    (let [queue   (init-queue ids)
          settled (transient (imap/int-set))]
      (produce! graph value arcs f queue settled)))
  ;; ------
  IReduceInit
  (reduce [_ rf init]
    (loop [ret     init
           queue   (init-queue ids)
           settled (transient (imap/int-set))]
      (let [trace (poll-unsettled! queue settled)]
        (if (nil? trace) ret ;; empty queue
          (let [rr (rf ret trace)]
            (if (reduced? rr) @rr
              (recur rr
                     (relax-nodes! value f (arcs (get graph (key trace))) trace queue)
                     (conj! settled (key trace)))))))))
  ;; ------
  IReduce
  (reduce [_ rf]
    (loop [ret     nil
           queue   (init-queue ids)
           settled (transient (imap/int-set))]
      (let [trace (poll-unsettled! queue settled)]
        (if (nil? trace) ret ;; empty queue
          (let [next-queue   (relax-nodes! value f (arcs (get graph (key trace))) trace queue)
                next-settled (conj! settled (key trace))]
            (case (count settled)
              (0 1) (recur ret next-queue next-settled) ;; ignore ret and keep making items
              2     (let [previous (rp/path trace)] ;; call rf with the first two items in coll
                      (recur (apply rf previous) next-queue next-settled))
              (let [rr (rf ret trace)] ;;default branch
                (if (reduced? rr) @rr
                  (recur rr next-queue next-settled)))))))))
  ;; declaring as Sequential will cause the seq to be used for nth, etc
  Sequential)