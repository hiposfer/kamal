(ns backend.routing.core
  (:require [clojure.data.int-map :as imap])
  (:import (java.util PriorityQueue Queue)
           (clojure.lang IReduceInit))
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
(def directions #{::forward ::backward})

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
;;       carrying to much information simulatanously. For example if you just want
;;       to know the id and worth of an element, having more than that is only noise.
;;       This way (hopefully) allows the algorithm developer to leverage more flexibility
;;       while providing maximum performance
(defprotocol Identifiable
  (id [this] "the node id associated to this element"))

;; ------ special protocols for Dijkstra graph traversal
(defprotocol Traceable
  (worth [this] "see Worth protocol")
  (path  [this] "the sequence of nodes taken to get here"))

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
  (cost [this] value)
  (time [this] value)
  (sum  [this that] (SimpleValue. (+ value (.-value ^SimpleValue that)))))

(deftype IdentifiableTrace [^long id trace path]
  Identifiable
  (id [this] id)
  Traceable
  (worth [this] trace)
  (path  [this] path))

;; --------------------------------------------------------

(defn length
  [arc _]
  (let [val (/ (:length arc) (get (:kind arc) speeds min-speed))]
    (->SimpleValue val)))

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
    (run! (fn [id] (.add queue (->IdentifiableTrace id (->SimpleValue 0) '()))) init-set)
    queue))

(defn- poll-unsettled!
  "moves the queue's head to an unsettled node and returns it"
  [^PriorityQueue queue settled]
  (let [trace (.poll queue)]
    (cond
      (nil? trace) nil
      (contains? settled (id trace)) (recur queue settled)
      :return trace)))

; inspired by
; http://insideclojure.org/2015/01/18/reducible-generators/
(deftype Dijkstra [graph ids value arcs]
  ;clojure.lang.Seqable
  IReduceInit ;; NOTE: simple reduce?
  (reduce [this rf init]
    (loop [ret     init
           queue   (init-queue ids)
           settled (transient (imap/int-map))]
      (let [trace (poll-unsettled! queue settled)]
        (if (nil? trace) ret ;; empty queue
          (let [rr (rf ret trace)]
            (if (reduced? rr) @rr
              (do (reduce-kv (fn [_ dst arc]
                               (let [weight (sum (value arc trace)
                                                 (worth trace))]
                                 (.add queue (->IdentifiableTrace dst weight '()))))
                             queue
                             (arcs (get graph (id trace))))
                  (recur rr queue (assoc! settled (id trace) trace))))))))))

(defn dijkstra
  "returns a reducible object which yields inputs implementing Identifiable
   and Traceable. In other words something similar to
   {:id number :worth {:cost number :time number} :path traces}"
  [graph & {:keys [src direction worth]}]
  (let [arcs (condp = direction ::forward  successors
                                ::backward predecessors)]
    (->Dijkstra graph src worth arcs)))