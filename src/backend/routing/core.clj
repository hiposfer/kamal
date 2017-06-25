(ns backend.routing.core
  (:require [clojure.data.int-map :as imap])
  (:import (java.util PriorityQueue Queue)
           (clojure.lang IReduceInit Seqable Sequential IReduce ILookup)
           (java.io Writer))
  (:refer-clojure :exclude [time]))

;; TODO: consider replacing the uses of defprotocol with definterface+
;;       as described in https://github.com/ztellman/potemkin#definterface

;; ------------------- design constrainst --------------------------------;
; we are only interested in graphs that can be represented as a mapping of
; int -> Node. Since this makes the mapping easier in Clojure while
; keeping it immutable. We only allow a single arc per dst/src node a.k.a
; simple graph, therefore we purposedly ignore multi-graph and pseudo-graphs
; see http://mathworld.wolfram.com/Graph.html

; We assume that all routings are time-dependent even if they are not, in which
; case the user can simply ignore it. Furthermore we assume that all routings
; can be completely determined by a cost function regardless of how that cost
; is calculated.

;; TODO: transform to meters/second
;; in km/h
(def speeds
  "Mapping of OSM-highway type to speed trying to follow both
  http://wiki.openstreetmap.org/wiki/Map_Features#Highway and
  http://wiki.openstreetmap.org/wiki/OSM_tags_for_routing/Maxspeed as close as
  possible"
  {::motorway 110,      ::trunk 110,        ::primary 70,      ::secondary 60
   ::tertiary 50,       ::motorway_link 50, ::trunk_link 50,   ::primary_link 50
   ::secondary_link 50, ::road 40,          ::unclassified 40, ::residential 30
   ::unsurfaced 30,     ::living_street 10, ::service 5})

(def min-speed 1) ;;km/h

;; Protocols for Inductive graph implementation as defined by Martin Erwig
;; https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
(defprotocol View ;; in FGL this is called a Decomposition
  (context [this] "a one step inductive graph extension")
  (graph   [this] "the rest of the graph"))

(defprotocol Context ;; in FGL this also includes the Node id and its labels
  (predecessors [this] "the incoming arcs of this node under the current view")
  (successors   [this] "the outgoing arcs of this node under the current view"))
  ; I intentionally left the `label` function out since Clojure already provides a way
  ; to retrieve information from an Object; the `get` function. If you want to have that
  ; functionality simply implement the `Clojure.lang.Ilookup` interface

;; NOTE: I intentionally left the ID out of the Context protocol in order to avoid
;;       carrying to much information simultaneously. For example if you just want
;;       to know the id and worth of an element, having more than that is only noise.
;;       This way (hopefully) allows the algorithm developer to leverage more flexibility
;;       while providing maximum performance
(defprotocol Identifiable
  (id [this] "the node id associated to this element"))

;; ------ special protocols for Dijkstra graph traversal
(defprotocol Traceable
  (worth [this] "see Worth protocol")
  (path  [this] "the sequence of Identifiable elements taken to get here"))

(defprotocol Valuable
  (cost [this] "a number indicating how difficult it is to get to a specific node")
  (time [this] "a number indicating how much time it takes to get to a specific node")
  (sum [this that] "adds two valuables into one"))

;; ------------------------------------------------------
; graph is an {id node}
; Node is a {:lon :lat :out-arcs {dst-id arc} :in-arcs {src-id arc}}
; Arc is a {:src node-id :dst node-id :length meters :kind OSM-highway-type}
(defrecord Node [^double lon ^double lat out-arcs in-arcs]
  Context
  (predecessors [this] (:in-arcs this))
  (successors   [this] (:out-arcs this)))

(defrecord Arc  [^long src ^long dst ^double length kind])


(deftype SimpleValue [^double value]
  Valuable
  (cost [_] value)
  (time [_] value)
  (sum  [_ that] (SimpleValue. (+ value (.-value ^SimpleValue that))))
  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [_ k default]
    (case k
      :cost value
      :time value
      default)))

(defmethod print-method SimpleValue [^SimpleValue v ^Writer w]
  (.write w (str "{:cost " (:cost v) " "
                 ":time " (:time v) " }")))

(deftype IdentifiableTrace [^long id footprint prior]
  Identifiable
  (id [_] id)
  Traceable
  (worth [_] footprint)
  (path  [this]
    (if (nil? prior) (list this)
      (cons this (lazy-seq (path prior)))))
  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k default]
    (case k
      :id    id
      :worth footprint
      :path  (path this)
      default)))


(defmethod print-method IdentifiableTrace [^IdentifiableTrace v ^Writer w]
  (.write w (str "{:id " (:id v) " "
                   ":worth " (:worth v) " "
                   ":path " (:path v) " }")))

;; --------------------------------------------------------

; travis-ci seems to complaint about not finding a matching constructor if the
; init size is not there. Funnily the ctor with a single comparator is not defined
; in the java docs .... hmmm :/
(defn- init-queue
  "Returns a new MUTABLE priority queue and adds all the sources id to
  the beginning of the queue."
  ^Queue
  [init-set]
  (let [cheapest-path (fn [trace1 trace2] (compare (cost (worth trace1))
                                                   (cost (worth trace2))))
        queue  ^Queue (new PriorityQueue 10 cheapest-path)]; 10 init size
    (run! (fn [id] (.add queue (->IdentifiableTrace id (->SimpleValue 0) nil))) init-set)
    queue))

(defn- poll-unsettled!
  "moves the queue's head to an unsettled node id and returns the element
  containing it"
  [^PriorityQueue queue settled]
  (let [trace (.poll queue)]
    (cond
      (nil? trace) nil
      (contains? settled (id trace)) (recur queue settled)
      :return trace)))

(defn- step!
  "utility function: DO NOT USE DIRECTLY.
  polls the next unsettled trace from the queue and adds all its neighbours
  to it"
  [graph settled value arcs ^Queue queue]
  (let [trace (poll-unsettled! queue settled)]
    (when trace
      (reduce-kv (fn [_ dst arc]
                     (let [weight (sum (value arc trace)
                                       (worth trace))]
                       (.add queue (->IdentifiableTrace dst weight trace))))
                 queue
                 (arcs (get graph (id trace))))
      trace)))

(defn- produce!
  "utility function: DO NOT USE DIRECTLY
  returns a lazy sequence of traces by sequentially mutating the
  queue (step!(ing) into it) and concatenating the latest poll with
  the rest of them"
  [graph value arcs ^Queue queue settled trace]
  (if (nil? trace) (list)
    (cons trace
          (lazy-seq (produce! graph value arcs queue (assoc! settled (id trace) trace)
                                                     (step! graph settled value arcs queue))))))

; inspired by
; http://insideclojure.org/2015/01/18/reducible-generators/
(deftype Dijkstra [graph ids value arcs]
  Seqable
  (seq [_]
    (let [queue   (init-queue ids)
          settled (transient (imap/int-map))]
      (produce! graph value arcs queue settled (step! graph settled value arcs queue))))
  ;; ------
  IReduceInit
  (reduce [_ rf init]
    (loop [ret     init
           queue   (init-queue ids)
           settled (transient (imap/int-map))]
      (let [trace (step! graph settled value arcs queue)]
        (if (nil? trace) ret ;; empty queue
          (let [rr (rf ret trace)]
            (if (reduced? rr) @rr
              (recur rr queue (assoc! settled (id trace) trace))))))))
  ;; ------
  IReduce
  (reduce [_ rf]
    (loop [ret     ::unknown
           queue   (init-queue ids)
           settled (transient (imap/int-map))]
      (let [trace (step! graph settled value arcs queue)]
        (cond
          (nil? trace) ret ;; empty queue
          (= 2 (count settled)) (let [previous (path trace)] ;; call rf with the first two items in coll
                                  (recur (apply rf previous)
                                         queue
                                         (assoc! settled (id trace) trace)))
          (> 2 (count settled)) (recur ret queue (assoc! settled (id trace) trace)) ;; ignore ret and keep making items
          :default ;; normal reduction steps, just like in IReduceInit
          (let [rr (rf ret trace)]
            (if (reduced? rr) @rr
              (recur rr queue (assoc! settled (id trace) trace))))))))
  ;; declaring as Sequential will cause the seq to be used for nth, etc
  Sequential)

(defn dijkstra
  "returns a reducible sequence which yields inputs implementing Identifiable
   and Traceable. In other words something similar to
   {:id number :worth {:cost number :time number} :path traces}

  Parameters:
   - :worth is a function that takes an Arc and an Identifiable Trace
            and returns a Valuable from Arc src to dst
   - :src is a set of node ids to start searching from
   - :direction is one of ::forward or ::backward and determines whether
     to use the outgoing or incoming arcs of each node"
  [graph & {:keys [src direction worth]}]
  (let [arcs (condp = direction ::forward  successors
                                ::backward predecessors)]
    (->Dijkstra graph src worth arcs)))