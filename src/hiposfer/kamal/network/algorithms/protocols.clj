(ns hiposfer.kamal.network.algorithms.protocols)


(defprotocol Passage
  (way [this] "return the way id that a Link is associated with"))

;; ------ special protocols for Dijkstra network traversal
(defprotocol Valuable "A simple representation of a generic routing worth function result"
  (cost [this]     "a number indicating how difficult it is to get to a specific node")
  (sum [this that] "adds two Valuable implementations. This way a Valuable implementation
                    doesnt need to be a Number"))

(defprotocol GeoCoordinate
  (lat [this] "latitude in decimal numbers")
  (lon [this] "longitude in decimal numbers"))
