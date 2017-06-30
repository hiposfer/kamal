(ns backend.routing.protocols
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

;; Protocols for Inductive graph implementation as defined by Martin Erwig
;; https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
;(defprotocol View ;; in FGL this is called a Decomposition
;  (context [this] "a one step inductive graph extension")
;  (graph   [this] "the rest of the graph"))

(defprotocol Context ;; in FGL this also includes the Node id and its labels
  (predecessors [this] "the incoming arcs of this node under the current view")
  (successors   [this] "the outgoing arcs of this node under the current view"))
; NOTES:
; I intentionally left the `label` function out since Clojure already provides a way
; to retrieve information from an Object; the `get` function. If you want to have that
; functionality simply implement the `Clojure.lang.Ilookup` interface

; I intentionally left the ID out of the Context protocol in order to avoid
; carrying to much information simultaneously. For example if you just want
; to know the id and worth of an element, having more than that is only noise.
; This way (hopefully) allows the algorithm developer to leverage more flexibility
; while providing maximum performance

; a possible way to implement a Context in an IntMap is to use clojure's
; `find` function to get the MapEntry which includes the id and value
; and use `get` to only get the value. That way the context can be separated
; from the Id without losing information

;; ------ special protocols for Dijkstra graph traversal
(defprotocol Traceable "Protocol for elements that can produce a sequence
  of other elements of the same type which were traversed before arriving to this one"
  (path  [this] "the sequence of Identifiable elements taken to get here"))

(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this] "a number indicating how difficult it is to get to a specific node")
  (time [this] "a number indicating how much time it takes to get to a specific node")
  (sum [this that] "adds two valuables into one"))
