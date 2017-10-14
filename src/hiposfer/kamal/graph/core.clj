(ns hiposfer.kamal.graph.core
  (:require [hiposfer.kamal.graph.protocols :as rp]
            [clojure.data.int-map :as imap])
  (:import (java.util Map$Entry)
           (clojure.lang IPersistentMap Seqable IReduceInit IReduce Sequential ITransientSet IPersistentVector)
           (clojure.data.int_map PersistentIntMap)
           (org.teneighty.heap FibonacciHeap Heap)))

;; ----- utility functions
(defn node? [coll] (and (satisfies? rp/Context coll)
                        (satisfies? rp/Binder coll)
                        (satisfies? rp/Incoherent coll)))

(defn edge? [coll] (and (satisfies? rp/Link coll)
                        (satisfies? rp/UndirectedLink coll)))

(defn arc? [coll] (satisfies? rp/Link coll))

(defn graph? [coll] (and (satisfies? rp/Coherent coll) ;; connect
                         (satisfies? rp/Incoherent coll) ;; disconnect
                         (instance? IPersistentMap coll))) ;; assoc, dissoc, contains, get

(defn nodes
  "returns the node objects (no id) of graph. The order of the elements is not
   guaranteed"
  [graph]
  (vals graph))

(defn successors
  "returns a lazy sequence of outgoing arcs. The order of the elements is not
  guaranteed"
  [graph]
  (sequence (map rp/successors) (vals graph)))

(defn predecessors
  "returns a lazy sequence of incoming arcs. The order of the elements is not
  guaranteed"
  [graph]
  (sequence (map rp/predecessors) (vals graph)))

(defn edges
  "returns a lazy sequence of bidirectional arcs (those that conform to edge?)
  Duplicates are removed from the sequence"
  [graph]
  (sequence (comp (mapcat #(concat (rp/successors %) (rp/predecessors %)))
                  (filter edge?)
                  (distinct))
            (vals graph)))

;; -------------------------------
; graph is an {id node}
; Node is a {:lon :lat :arcs {node-id arc}}
; Arc is a {:src :dst :way-id}

;; TODO: do I need the :mirror? flag?
(defrecord Edge [^long src ^long dst ^long way]
  rp/Link
  (src [_] src)
  (dst [_] dst)
  (mirror [_] (map->Edge {:src dst :dst src :way way})) ;:mirror? true}))
  ;(mirror? [this] (:mirror? this))
  rp/Passage
  (way [_] way)
  rp/UndirectedLink)

(defrecord Arc [^long src ^long dst ^long way]
  rp/Link
  (src [_] src)
  (dst [_] dst)
  (mirror [_] (map->Arc {:src dst :dst src :way way})); :mirror? true}))
  ;(mirror? [this] (:mirror? this))
  rp/Passage
  (way [_] way))

;; A NodeInfo is an element that can be included in a graph
;; for routing purposes
;; NOTE: for (memory) convenience we represent the edges as a directed outgoing arc
;;       and reverse it only if necessary (see predecesors)
;; This implementation of NodeInfo only accepts one Edge/Arc per src/dst node
(defrecord NodeInfo [^double lon ^double lat outgoing incoming]
  rp/Context
  (predecessors [this] (concat (sequence (comp (filter edge?)
                                               (map rp/mirror))
                                         (vals (:outgoing this)))
                               (vals (:incoming this))))
  (successors   [this] (concat (sequence (comp (filter edge?)
                                               (map rp/mirror))
                                         (vals (:incoming this)))
                               (vals (:outgoing this))))
  rp/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon)
  rp/Binder
  (inbound  [this arc-or-edge]
    (assoc-in this [:incoming (rp/src arc-or-edge)] arc-or-edge))
  (outbound [this arc-or-edge]
    (assoc-in this [:outgoing (rp/dst arc-or-edge)] arc-or-edge))
  rp/Incoherent
  (disconnect [this arc-or-edge]
    (-> (update this :outgoing dissoc (rp/src arc-or-edge))
        (update      :incoming dissoc (rp/dst arc-or-edge)))))

;; we use a persistent Int Map as graph representation since it is fast
;; and memory efficient
(extend-type PersistentIntMap
  rp/Coherent
  (connect [graph arc-or-edge] ;; we assume that both src and dst nodes exists
    (let [graph2 (update graph (rp/src arc-or-edge) rp/outbound arc-or-edge)]
      (if (edge? arc-or-edge) ;; arc otherwise
        (update graph2 (rp/dst arc-or-edge) rp/inbound arc-or-edge)
        (update graph2 (rp/dst arc-or-edge) rp/inbound (rp/mirror arc-or-edge)))))
  rp/Incoherent
  (disconnect [graph arc-or-edge] ;; we assume that both src and dst nodes exists
    (let [src (rp/src arc-or-edge)
          dst (rp/dst arc-or-edge)]
      (-> (update graph src rp/disconnect src dst)
          (update       dst rp/disconnect dst src)))))

;; a Point is a simple longitude, latitude pair used to
;; represent the geometry of a way
(defrecord Point [^double lon ^double lat]
  rp/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon))

;; useful for geojson coordinates manipulation
(extend-type IPersistentVector
  rp/GeoCoordinate
  (lat [this] (second this))
  (lon [this] (first this)))

;; this is specially useful for generative testing: there we use generated
;; nodes and arcs. Here we prefer distinguishing between arcs and edges
;; explicitly as performance is not an issue
(extend-type IPersistentMap
  rp/Context
  (predecessors [this] (concat (sequence (comp (filter edge?)
                                               (map rp/mirror))
                                         (vals (:outgoing this)))
                               (vals (:incoming this))))
  (successors   [this] (concat (sequence (comp (filter edge?)
                                               (map rp/mirror))
                                         (vals (:incoming this)))
                               (vals (:outgoing this))))
  rp/GeoCoordinate
  (lat [this] (:lat this))
  (lon [this] (:lon this))
  rp/Binder
  (inbound  [this arc-or-edge]
    (assoc-in this [:incoming (rp/src arc-or-edge)] arc-or-edge))
  (outbound [this arc-or-edge]
    (assoc-in this [:outgoing (rp/dst arc-or-edge)] arc-or-edge))
  rp/Incoherent
  (disconnect [this arc-or-edge]
    (-> (update this :outgoing dissoc (rp/src arc-or-edge))
        (update      :incoming dissoc (rp/dst arc-or-edge))))
  ;; arc representation
  rp/Link
  (src [this] (:src this))
  (dst [this] (:dst this))
  (mirror [this] (assoc this :src (:dst this)
                             :dst (:src this)))
                             ;:mirror? true))
  ;(mirror? [this] (:mirror? this))
  rp/Passage
  (way [this] (:way this)))

;; the simplest way to represent the Cost in a graph traversal
(extend-type Number
  rp/Valuable
  (cost [this] this)
  (sum  [this that] (+ this that)))

; a Trace is MapEntry-like object used to map a node id to a cost without
; using a complex data structure like a hash-map. It is recursive since
; it contains a reference to its previous trace. Thus only the
; current instance is necessary to determine the complete path traversed up
; until it.
(deftype Trace [^long id value prior]
  rp/Traceable
  (path [this]
    (if (nil? prior) (list this)
      (cons this (lazy-seq (rp/path prior)))))
  ; Interface used by Clojure for the `key` and `val` functions. Those
  ; functions are expected to work for any key-value structure. We
  ; implement it here for convenience since a Trace maps a node id
  ; to its cost.
  Map$Entry ; https://docs.oracle.com/javase/7/docs/api/java/util/Map.Entry.html
  (getKey [_] id)
  (getValue [_] value)
  (setValue [_ _] (throw (ex-info "Unsupported Operation" {} "cannot change an immutable value")))
  ; we rely on the key and val implementing their own equals
  (equals [this that]
    (let [t1 (rp/path this)
          t2 (rp/path that)]
      (and (apply = (map key t1) (map key t2))
           (apply = (map val t1) (map val t2)))))
  (hashCode [_] (hash [id value prior]))
  Object
  (toString [_] (str "[" id " " value "]")))


;; ------------------ DIJKSTRA CORE -------------------
(defn- init-queue
  "Returns a new MUTABLE fibonacci heap (priority queue) and adds all the sources id to
  the beginning of the queue."
  ^Heap [init-set]
  (let [queue  ^Heap (new FibonacciHeap)]
    (run! (fn [id] (.insert queue 0 (->Trace id 0 nil)))
          init-set)
    queue))

;; This is a hack taken from the Efficient Route Planning Course in Freiburg
;; Instead of decreasing the priority of an element in (relax-nodes!) we
;; check if the next element was already settled and ignore it if so.
;; This is not very efficient but it shouldnt impact the performance
;; since real world road networks a 1/3 node/arc ratio.
;; https://ad-wiki.informatik.uni-freiburg.de/teaching/EfficientRoutePlanningSS2012
(defn- poll-unsettled!
  "moves the queue's head to an unsettled node id and returns the trace
  containing it"
  [^Heap queue ^ITransientSet settled] ;; BUG: https://dev.clojure.org/jira/browse/DIMAP-14
  (let [entry (.extractMinimum queue)]
    (if (.contains settled (key (.getValue entry)))
      (recur queue settled)
      (.getValue entry))))

(defn- relax-nodes!
  "adds all node-arcs to the queue"
  [^ITransientSet settled value f node-arcs trace ^Heap queue]
  (let [arc (first node-arcs)]
    (if (nil? arc) queue
      (if (.contains settled (f (first node-arcs)))
        (recur settled value f (rest node-arcs) trace queue)
        (let [weight (rp/sum (value arc trace)
                             (val trace))]
          (.insert queue weight (->Trace (f arc) weight trace))
          (recur settled value f (rest node-arcs) trace queue))))))

(defn- produce!
  "returns a lazy sequence of traces by sequentially mutating the
  queue (step!(ing) into it) and concatenating the latest poll with
  the rest of them"
  [graph value arcs f ^Heap queue settled]
  (if (.isEmpty queue) (list)
    (let [trace (poll-unsettled! queue settled)]
      (let [next-queue   (relax-nodes! settled value f (arcs (graph (key trace)))
                                       trace queue)
            next-settled (conj! settled (key trace))]
        (cons trace
              (lazy-seq (produce! graph value arcs f next-queue next-settled)))))))


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
      (if (.isEmpty queue) ret
        (let [trace (poll-unsettled! queue settled)
              rr (rf ret trace)]
            (if (reduced? rr) @rr
              (recur rr
                     (relax-nodes! settled value f (arcs (graph (key trace)))
                                   trace queue)
                     (conj! settled (key trace))))))))
  ;; ------
  IReduce
  (reduce [this rf] (.reduce ^IReduceInit this rf (rf)))
  ;; declaring as Sequential will cause the seq to be used for nth, etc
  Sequential)