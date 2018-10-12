(ns hiposfer.kamal.network.algorithms.protocols)

;; ------ special protocols for Dijkstra network traversal
(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this]     "a number indicating how difficult it is to get to a specific node")
  (sum [this that] "adds two Valuable implementations. This way a Valuable implementation
                    doesnt need to be a Number"))

(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))

(defprotocol Router
  "An instance used to direct the movement of Dijkstra's traversal algorithm"
  (relax [this node trail]
    "attempts to relax node following trail path. Returns a Valuable implementation")
  (successors [this node]
    "returns a sequence of node that can be reached from node"))
