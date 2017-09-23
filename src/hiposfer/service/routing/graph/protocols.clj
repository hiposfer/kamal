(ns hiposfer.service.routing.graph.protocols)

;; TODO: consider replacing the uses of defprotocol with definterface+
;;       as described in https://github.com/ztellman/potemkin#definterface

;; ------------------- design constrainst --------------------------------;
; we are only interested in graphs that can be represented as a mapping of
; int -> Node. Since this makes the mapping easier in Clojure while
; keeping it immutable. Although we focus on a single-arc-per src/dst model
; we represent arcs as a list of edges, thus multiple arcs between the same
; src and dst are possible for a single node.
; see http://mathworld.wolfram.com/Graph.html

; We assume that routing are time-dependent even if they are not, in which
; case the user can simply ignore it. Furthermore we assume that routing
; can be completely determined by a cost function regardless of how that cost
; is calculated.

;; Protocols for Inductive graph implementation inspired by Martin Erwig
;; https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
;; this implementation (currently) is not an inductive Graph. Maybe in the future?
(defprotocol View ;; in FGL this is called a Decomposition
  (context [this] "a one step inductive graph extension")
  (graph   [this] "the rest of the graph"))

(defprotocol Context ;; in FGL this also includes the Node id and its labels
  (predecessors [this] "returns a sequence of incoming arcs of this node under the current view")
  (successors   [this] "returns a sequence of outgoing arcs of this node under the current view"))
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
  (path  [this] "sequence of Traceable elements taken to get to this one (in reverse order)"))

(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this] "a number indicating how difficult it is to get to a specific node")
  (sum [this that] "adds two valuables into one. Needed to avoid making Valuable
                    implementations more than just numbers."))

(defprotocol Arc
  (src [this] "the start node id of an Arc")
  (dst [this] "the destination node id of an Arc"))

;; todo: do we need a separate protocol for routable? should this be included in the Arc
(defprotocol Routable
  (way [this] "return the way id that an Arc is associated with"))

(defprotocol Reversible
  (mirror [this] "returns an Arc in the opposite direction than the original"))

(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))