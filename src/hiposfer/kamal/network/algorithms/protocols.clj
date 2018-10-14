(ns hiposfer.kamal.network.algorithms.protocols)

;; ------ special protocols for Dijkstra network traversal
(defprotocol Valuable
  "A simple representation of a generic routing worth function result"
  (cost [this] "a number indicating how difficult it is to get to a specific node"))

(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))

(defprotocol Arc "a graph arc/edge representation"
  (src [this] "the origin node of this link")
  (dst [this] "the destination node of this link"))

(defprotocol Bidirectional
  (mirror [this] "swap the src and dst of this Link")
  (mirror? [this] "returns true if this Link is a mirror of its original"))

(defprotocol Node
  #_(id           [this] "returns a unique identifier for this Context")
  #_(predecessors [this] "returns the Links that point to this Context") ;TODO
  (successors   [this] "returns the Links that origin at this Context"))

(defprotocol Router
  "An instance used to direct the movement of Dijkstra's traversal algorithm"
  #_(seed  [this] ;TODO
      "returns a sequence of [Node Valuable] that will be ")
  (relax [this arc trail]
    "attempts to relax node following trail path. Returns a Valuable implementation"))
