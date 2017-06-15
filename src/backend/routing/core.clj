(ns backend.routing.core
  (:require [clojure.data.int-map :as imap]
            [clojure.set :as set]))


;; TODO: consider replacing the uses of defprotocol with definterface+
;;       as described in https://github.com/ztellman/potemkin#definterface


; ------------------------ GRAPH ELEMENTS -------------------------------;
; graph is an {id node}
; Node is a {:lon :lat :dst {dst-id arc} :src {src-id arc}}
; Arc is a {:src node-id :dst node-id :length meters :kind OSM-highway-type}
(defrecord Node [^double lon ^double lat out-arcs in-arcs])
(defrecord Arc  [^long src ^long dst ^double length kind])

;; --- design constrainst ----;
; we are only interested in graphs that can be represented as a mapping of
; int -> Node. Since this makes the mapping faster/efficient in Clojure while
; keeping it immutable. We only allow a single arc per dst/src node a.k.a simple
; graph, therefore we purposedly ignore multi-graph and pseudo-graphs; see
; http://mathworld.wolfram.com/Graph.html
; We assume that all routings are time-dependent even if they are not, in which
; case the user can simply ignore it. Furthermore we assume that all routings
; can be completely determined by a cost function regardless of how that cost
; is calculated.


; --------------- UTILITY Elements for Graph Traversal ------------------;
(defrecord Trace [^double cost ^double time previous])

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

(def directions
  "the possible traversal directions of a Dijkstra algorithm. Note that
   ::bidirectional executes a single-thread bidirectional traversal by interleaving
   the forward and backward search whereas ::parallel executes a multi-thread (2)
   traversal"
  #{::forward ::backward ::bidirectional}) ;::parallel})

;; ----------- ROUTERS for Graph traversal ------------------------------;
;; Search from One src to One dst
(defrecord ArcLengthRouter [^long src ^long dst direction])
;; Search from N sources to N destinations
;(defrecord ArcLengthSetRouter [sources destinations direction])

(defprotocol GraphTraversal
  (sources [this] "the set of node ids from which Dijkstra's algorithm would start
                   in a fordward traversal")
  (destinations [this] "the set of node ids from which Dijkstra's algorithm would start
                        in a backward traversal")
  (direction [this] "the direction in which the Dijkstra's traversal is executed.
                     See backend.routing.core/directions")
  (worth [this arc settled] "returns a tuple [cost time] for traversing this Arc")
  (stop? [this settled last-settled]
         [this settled-src curr-forward settled-dst curr-backward] "stop relaxing arcs?"))

;; ------------ IMPLEMENTATIONS ---------------------------------;
(extend-protocol GraphTraversal
  ArcLengthRouter
  (sources [this] (imap/int-set [(:src this)]))
  (destinations [this] (imap/int-set [(:dst this)]))
  (direction [this] (:direction this))
  (worth [this arc _]
    (let [val (/ (:length arc) (get (:kind arc) speeds min-speed))]
      (vector-of :double val val)))
  (stop?
    ([this settled settling]
     (if (= ::forward (:direction this))
       (= settling (:dst this))
       (= settling (:src this))))
    ([this forward cforth backward cback]
     (or (contains? forward cback)
         (contains? backward cforth)))))
  ;ArcLengthSetRouter
  ;(sources [this] (imap/int-set (:sources this)))
  ;(destinations [this] (imap/int-set (:destinations this)))
  ;(direction [this] (:direction this))
  ;(worth [this arc _]
  ;  (let [val (/ (:length arc) (get (:kind arc) speeds min-speed))]
  ;    (vector-of :double val val)))
  ;(stop? ([this settled] (every? #(contains? settled %) (:destinations this)))
  ;  ;TODO
  ;       ([this _ _] (throw (new NoSuchMethodException("I havent implemented this yet"))))))
