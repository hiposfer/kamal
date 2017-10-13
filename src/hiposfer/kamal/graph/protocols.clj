(ns hiposfer.kamal.graph.protocols)

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
;; TODO: this implementation (currently) is not an inductive Graph. Maybe in the future?
;(defprotocol View ;; in FGL this is called a Decomposition
;  (context [this] "a one step inductive graph extension")
;  (graph   [this] "the rest of the graph"))

(defprotocol Context ;; in FGL this also includes the Node id and its labels
  (predecessors [this] "returns a sequence of incoming arcs of this node under the current view")
  (successors   [this] "returns a sequence of outgoing arcs of this node under the current view"))
; NOTES:
; I intentionally left the `label` function out since Clojure already provides a way
; to retrieve information from an Object; the `get` function. If you want to have that
; functionality simply implement the `Clojure.lang.Ilookup` interface

; I intentionally left the ID out of the Context protocol in order to avoid
; carrying to much information simultaneously.
; This way (hopefully) allows the algorithm developer to leverage more flexibility
; while providing maximum performance

; If you want to get the id of a node together with its data, please use Clojure's
; `find` function to get the MapEntry which includes the id and value.
; Use `get` to only get the value. That way the context can be separated
; from the Id without losing information

;; ------ special protocols for Dijkstra graph traversal
(defprotocol Traceable "Protocol for elements that can produce a sequence
  of other elements of the same type which were traversed before arriving to this one"
  (path  [this] "sequence of Traceable elements taken to get to this one (in reverse order)"))

(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this]     "a number indicating how difficult it is to get to a specific node")
  (sum [this that] "adds two Valuable implementations. This way a Valuable implementation
                    doesnt need to be a Number"))


;; ------ protocols for Edge & Arcs
(defprotocol Link "A connection between two nodes. Directed or undirected"
  (src [this] "the start node id of a Link")
  (dst [this] "the destination node id of a Link")
  (mirror [this] "returns a Link in the opposite direction than the original"))
  ;(mirror? [this]))

(defprotocol Passage
  (way [this] "return the way id that a Link is associated with"))

;; TODO: does this need to be a flag protocol? should it rather be a method?
(defprotocol UndirectedLink "A flagging protocol to distinguish Arcs from Edges")

;; ------- protocols for Nodes
(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))

;; ------- protocols for Graphs & Nodes
(defprotocol Coherent "having a natural or due agreement of parts; harmonious"
  (connect [coll arc-or-edge] "connect two nodes in a graph"))

(defprotocol Incoherent
  (disconnect [coll arc-or-edge] "disconnect two nodes in a graph"))

(defprotocol Binder
  (inbound  [node arc-or-edge] "bind the given incoming Link to this Node")
  (outbound [node arc-or-edge] "bind the given outgoing Link to this Node"))