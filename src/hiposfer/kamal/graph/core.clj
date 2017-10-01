(ns hiposfer.kamal.graph.core
  (:require [hiposfer.kamal.graph.protocols :as rp]
            [clojure.data.int-map :as imap])
  (:import (java.util Map$Entry Queue PriorityQueue)
           (clojure.lang IPersistentMap Seqable IReduceInit IReduce Sequential ITransientSet IPersistentVector Associative)
           (clojure.data.int_map PersistentIntMap)))

;; ----- utility functions
(defn node? [coll] (and (satisfies? coll rp/Context)
                        (satisfies? coll rp/Coherent)))

(defn edge? [coll] (and (satisfies? coll rp/Link)
                        (satisfies? coll rp/UndirectedLink)))

(defn arc? [coll] (satisfies? coll rp/Link))

(defn graph? [coll] (and (satisfies? coll rp/Coherent) ;; connect, disconnect
                         (instance? IPersistentMap coll) ;; assoc, dissoc, contains, get
                         (satisfies? coll rp/Queryable))) ;; query

;; -------------------------------
; graph is an {id node}
; Node is a {:lon :lat :arcs {dst-id arc}}
; Arc is a {:src :dst :way-id}
(defrecord Edge [^long src ^long dst ^long way]
  rp/Link
  (src [_] src)
  (dst [_] dst)
  (mirror [_] (map->Edge {:src dst :dst src :way way :mirror? true}))
  (mirror? [this] (:mirror? this))
  rp/Passage
  (way [_] way)
  rp/UndirectedLink)

(defrecord Arc [^long src ^long dst ^long way]
  rp/Link
  (src [_] src)
  (dst [_] dst)
  (mirror [_] (map->Arc {:src dst :dst src :way way :mirror? true}))
  (mirror? [this] (:mirror? this))
  rp/Passage
  (way [_] way))

;; A NodeInfo is an element that can be included in a graph
;; for routing purposes
;; NOTE: for (memory) convenience we represent the edges as a directed outgoing arc
;;       and reverse it only if necessary (see predecesors)
;; This implementation of NodeInfo only accepts one Edge/Arc per dst node
(defrecord NodeInfo [^double lon ^double lat arcs]
  rp/Context
  (predecessors [this] (concat (sequence (comp (filter edge?) (map rp/mirror))
                                         (vals (:arcs this)))
                               (vals (:in-arcs this))))
  (successors   [this] (vals (:arcs this)))
  rp/GeoCoordinate
  (lat [_] lat)
  (lon [_] lon)
  rp/Coherent
  (connect [this arc-or-edge]
    (cond
      (edge? arc-or-edge) (assoc-in this [:arcs (rp/dst arc-or-edge)] arc-or-edge)
      ;; -- arc otherwise
      ;incoming arc
      (rp/mirror? arc-or-edge) (assoc-in this [:in-arcs (rp/src arc-or-edge)] arc-or-edge)
      :outgoing (assoc-in this [:arcs (rp/dst arc-or-edge)] arc-or-edge)))
  (disconnect [this arc-or-edge]
    (let [src (rp/src arc-or-edge)
          dst (rp/dst arc-or-edge)]
      (-> (update this :arcs dissoc dst)
          (update      :in-arcs dissoc src)))))

;; we use a persistent Int Map as graph representation since it is fast
;; and memory efficient
(extend-type PersistentIntMap
  rp/Coherent
  (connect [graph arc-or-edge] ;; NOTE: we assume that both src and dst nodes exists
    (-> (update graph (rp/src arc-or-edge) rp/connect arc-or-edge)
        (update       (rp/dst arc-or-edge) rp/connect (rp/mirror arc-or-edge))))
  (disconnect [graph arc-or-edge] ;; NOTE: we assume that both src and dst nodes exists
    (let [src (rp/src arc-or-edge)
          dst (rp/dst arc-or-edge)]
      (-> (update graph src rp/disconnect src dst)
          (update       dst rp/disconnect dst src))))
  rp/Queryable
  (query [graph query] (throw (ex-info "this method has not been implemented yet!" query))))

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
  (predecessors [this] (concat (map rp/mirror (vals (:edges this)))
                               (vals (:in-arcs this))))
  (successors   [this] (concat (vals (:edges this))
                               (vals (:out-arcs this))))
  rp/Coherent
  (connect [this arc-or-edge]
    (cond
      (edge? arc-or-edge) (assoc-in this [:edges (rp/dst arc-or-edge)] arc-or-edge)
      ;; -- arc otherwise
      ;incoming arc
      (rp/mirror? arc-or-edge) (assoc-in this [:in-arcs (rp/src arc-or-edge)] arc-or-edge)
      :outgoing (assoc-in this [:out-arcs (rp/dst arc-or-edge)] arc-or-edge)))
  (disconnect [this arc-or-edge]
    (let [src (rp/src arc-or-edge)
          dst (rp/dst arc-or-edge)]
      (-> (update this :edges dissoc dst)
          (update      :out-arcs dissoc dst)
          (update      :in-arcs dissoc src))))
  rp/GeoCoordinate
  (lat [this] (:lat this))
  (lon [this] (:lon this))
  ;; arc representation
  rp/Link
  (src [this] (:src this))
  (dst [this] (:dst this))
  (mirror [this] (assoc this :src (:dst this)
                             :dst (:src this)
                             :mirror? true))
  (mirror? [this] (:mirror? this))
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
    (run! (fn [id] (.add queue (->Trace id 0 nil))) init-set)
    queue))

(defn- poll-unsettled!
  "moves the queue's head to an unsettled node id and returns the element
  containing it"
  [^Queue queue ^ITransientSet settled]
  (let [trace (.poll queue)]
    (if (nil? trace) nil
      (if (.contains settled (key trace))
        (recur queue settled)
        trace))))

(defn- relax-nodes!
  "polls the next unsettled trace from the queue and adds all its neighbours
  to it"
  [value f node-arcs trace ^Queue queue]
  (reduce (fn [_ arc]
            (let [weight (rp/sum (value arc trace)
                                 (val trace))]
              (.add queue (->Trace (f arc) weight trace))
              queue))
          queue
          node-arcs))

(defn- produce!
  "returns a lazy sequence of traces by sequentially mutating the
  queue (step!(ing) into it) and concatenating the latest poll with
  the rest of them"
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
      (let [trace (poll-unsettled! queue settled)]
        (if (nil? trace) ret ;; empty queue
          (let [rr (rf ret trace)]
            (if (reduced? rr) @rr
              (recur rr
                     (relax-nodes! value f (arcs (get graph (key trace))) trace queue)
                     (conj! settled (key trace)))))))))
  ;; ------
  IReduce
  (reduce [this rf] (.reduce ^IReduceInit this rf (rf)))
  ;; declaring as Sequential will cause the seq to be used for nth, etc
  Sequential)