(ns hiposfer.kamal.network.algorithms.protocols)

;; ------ special protocols for Dijkstra network traversal
(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this]     "a number indicating how difficult it is to get to a specific node"))

(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))

(defprotocol Router
  "An instance used to direct the movement of Dijkstra's traversal algorithm"
  (relax [this arc trail]
    "attempts to relax node following trail path. Returns a Valuable implementation")
  (arcs [this node]
    "returns a sequence of arcs that link node with its neighbours")
  (dst [this arc]
    "returns a node based on an arc"))
