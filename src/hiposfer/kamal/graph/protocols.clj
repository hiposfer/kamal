(ns hiposfer.kamal.graph.protocols)

;; TODO: consider replacing the uses of defprotocol with definterface+
;;       as described in https://github.com/ztellman/potemkin#definterface

;; ------------------- design constrainst --------------------------------;
; we are only interested in graphs that can be represented as a mapping of
; ID -> Node. Since this makes the mapping easier in Clojure while
; keeping it immutable.

; We assume that routing can be completely determined by a cost function
; regardless of how that cost is calculated.

;; ------ special protocols for Dijkstra graph traversal
(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this]     "a number indicating how difficult it is to get to a specific node")
  (sum [this that] "adds two Valuable implementations. This way a Valuable implementation
                    doesnt need to be a Number"))


;; ------ protocols for Edge & Arcs
(defprotocol Link "A connection between two nodes. Directed or undirected"
  (src [this] "the start node id of a Link")
  (dst [this] "the destination node id of a Link"))

(defprotocol Bidirectional
  (mirror [this] "returns a Link in the opposite direction of the original.
                  (= (mirror (mirror edge)) edge)")
  (mirror? [this] "returns a true if this Link is a mirror copy of the original"))

(defprotocol Passage
  (way [this] "return the way id that a Link is associated with"))

;; ------- protocols for Nodes
(defprotocol Context
  (predecessors [this] "returns a sequence of incoming arcs of this node")
  (edges        [this] "returns a sequence of bidirectional arcs of this node")
  (successors   [this] "returns a sequence of outgoing arcs of this node"))

(defprotocol Binder
  (inbound  [node link] "bind the given incoming Link to this Node")
  (outbound [node link] "bind the given outgoing Link to this Node"))

(defprotocol Incoherent
  (disconnect [node link] "disconnect two nodes in a graph"))

(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))