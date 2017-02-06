(ns hypobus.routing.core
  (:require [frechet-dist.protocols :as frepos]
            [hypobus.basics.geometry :refer [haversine]]))


; ------------------------ GRAPH ELEMENTS -------------------------------;
; node-map is an {id node}
; node-info is a {:lon :lat :out-arcs {dst-id arc} :in-arcs {src-id arc}}
(defrecord Node [^double lon  ^double lat  out-arcs in-arcs])
(defrecord Arc  [^long src-id ^long dst-id ^double length kind])


; --------------- UTILITY Elements for Graph Traversal ------------------------;
(defrecord Trace [^double cost previous])
;; TODO: transform to meters/second
;; in km/h
(def speeds {::motorway 110,      ::trunk 110,        ::primary 70,      ::secondary 60
             ::tertiary 50,       ::motorway_link 50, ::trunk_link 50,   ::primary_link 50
             ::secondary_link 50, ::road 40,          ::unclassified 40, ::residential 30
             ::unsurfaced 30,     ::living_street 10, ::service 5})

(def min-speed 1) ;;km/h



;; ----------- ROUTERS for Graph traversal -----------------------
;; Search from src to dst only by arc costs dst-ids is a set of ids
(defrecord SimpleDstRouter [^long dst-id])
;; Search from src to multiple dst only by arc costs
(defrecord MultiDstRouter [dst-ids])
;; Search from src to dst taking into account graph traversal restrictions
(defrecord ConstrainedDstRouter [graph ^long dst-id])
;; Search from src to multiple dst taking into account graph traversal restrictions
(defrecord ConstrainedMultiDstRouter [graph dst-ids])

(defprotocol GraphTraversal
  (cost [this arc traces])
  (stop?  [this traces] [this traces traces2])
  (return [this traces] [this traces traces2]))


;; ------------ IMPLEMENTATIONS ---------------------------------;
(extend-protocol frepos/Distance
  Node ;; graph node as a geographical point
  (frepos/distance [p1 p2]
    (haversine (Math/toRadians (:lon p1)) (Math/toRadians (:lat p1))
               (Math/toRadians (:lon p2)) (Math/toRadians (:lat p2)))))

(extend-protocol GraphTraversal
  SimpleDstRouter
  (cost [this arc _] (/ (:length arc) (get (:kind arc) speeds min-speed)))
  (stop? ([this traces] (contains? traces (:dst-id this)))
         ([this _ _] (throw (new NoSuchMethodException("I havent implemented this yet"))))) ;TODO
  (return ([this traces] (when (stop? this traces) traces)) ;; nil on unsuccessfull search
          ([this _ _] (throw (new NoSuchMethodException("I havent implemented this yet")))))) ;TODO
