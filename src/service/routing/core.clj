(ns service.routing.core
  (:require [service.routing.protocols :as rp]
            [clojure.data.int-map :as imap])
  (:import (java.util Map$Entry Queue PriorityQueue)
           (clojure.lang ILookup IPersistentMap Seqable IReduceInit IReduce Sequential ITransientSet)
           (java.io Writer)))

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
  (successors    [this] (:out-arcs this)))

;; TODO: missing interface/protocol definition here
(defrecord Arc  [^long src ^long dst ^double length kind])

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
  [value node-arcs trace ^Queue queue]
  (reduce-kv (fn [_ dst arc]
                 (let [weight (rp/sum (value arc trace)
                                      (val trace))]
                   (.add queue (->IdentifiableTrace dst weight trace))
                   queue))
             queue
             node-arcs))

(defn- produce!
  "returns a lazy sequence of traces by sequentially mutating the
  queue (step!(ing) into it) and concatenating the latest poll with
  the rest of them

  utility function: DO NOT USE DIRECTLY"
  [graph value arcs ^Queue queue settled]
  (let [trace (poll-unsettled! queue settled)]; (step! graph settled value arcs queue)
    (if (nil? trace) (list)
      (let [next-queue   (relax-nodes! value (arcs (get graph (key trace))) trace queue)
            next-settled (conj! settled (key trace))]
        (cons trace
              (lazy-seq (produce! graph value arcs next-queue next-settled)))))))



; inspired by http://insideclojure.org/2015/01/18/reducible-generators/
; A Collection type which can reduce itself faster than first/next traversal over its lazy
; representation.
(deftype Dijkstra [graph ids value arcs]
  Seqable
  (seq [_]
    (let [queue   (init-queue ids)
          settled (transient (imap/int-set))]
      (produce! graph value arcs queue settled)))
  ;; ------
  IReduceInit ;; TODO: try replacing the int-map with a bitset or hash-set from java
  (reduce [_ rf init]
    (loop [ret     init
           queue   (init-queue ids)
           settled (transient (imap/int-set))]
      (let [trace (poll-unsettled! queue settled)]
        (if (nil? trace) ret ;; empty queue
          (let [rr (rf ret trace)]
            (if (reduced? rr) @rr
              (recur rr
                     (relax-nodes! value (arcs (get graph (key trace))) trace queue)
                     (conj! settled (key trace)))))))))
  ;; ------
  IReduce
  (reduce [_ rf]
    (loop [ret     ::unknown
           queue   (init-queue ids)
           settled (transient (imap/int-set))]
      (let [trace (poll-unsettled! queue settled)]
        (if (nil? trace) ret ;; empty queue
          (let [next-queue   (relax-nodes! value (arcs (get graph (key trace))) trace queue)
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