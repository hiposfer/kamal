(ns hiposfer.kamal.network.graph.core
  "

  DEPRECATED !!


  Since the use of Datascript this namespace is not needed anymore

  But I am just too sad to delete it since I spent so much time designing the API

  I think that this is a pretty good Graph api though"
  (:require [hiposfer.kamal.network.graph.protocols :as rp]))

;; ----- utility functions
(defn node? [coll] (and (satisfies? rp/Context coll)
                        (satisfies? rp/Binder coll)
                        (satisfies? rp/Incoherent coll)))

;; NOTE: for convenience we represent the edges as a directed outgoing arc
;;       and reverse it only if necessary (see predecesors)
(defn edge? [coll] (and (satisfies? rp/Link coll)
                        (satisfies? rp/Bidirectional coll)))

(defn arc? [coll] (satisfies? rp/Link coll))

(def graph? "returns true if x implements IPersistentMap" map?)

(defn nodes
  "returns the node objects (no id) of graph. The order of the elements is not
   guaranteed"
  [graph]
  (vals graph))

(defn successors
  "returns a lazy sequence of outgoing arcs. The order of the elements is not
  guaranteed"
  [graph]
  (sequence (mapcat rp/successors) (vals graph)))

(defn predecessors
  "returns a lazy sequence of incoming arcs. The order of the elements is not
  guaranteed"
  [graph]
  (sequence (mapcat rp/predecessors) (vals graph)))

(defn edges
  "returns a lazy sequence of bidirectional arcs (those that conform to edge?)
  in their original direction i.e. no mirrored Links are returned. The order
  of the elements is not guaranteed"
  ([graph]
   (sequence (mapcat edges) (repeat graph) (keys graph)))
  ([graph node-id]
   (sequence (comp (filter edge?)
                   (map #(if (rp/mirror? %) (rp/mirror %) %)))
             (rp/successors (graph node-id)))))

(defn connect
  "connect the nodes src and dst of link"
  [graph link] ;; we assume that both src and dst nodes exists
  (-> (update graph (rp/src link) rp/outbound link)
      (update       (rp/dst link) rp/inbound  link)))

(defn disconnect
  "remove the link between src and dst of link"
  [graph link]
  (let [src (rp/src link)
        dst (rp/dst link)]
    (cond-> graph
      (contains? graph src) (update src rp/disconnect link)
      (contains? graph dst) (update dst rp/disconnect link))))

(defn attach
  "assoc id to node and connect all of its links to the graph. All nodes
  id used by the links MUST exist"
  [graph id node]
  (let [graph2 (assoc graph id node)
        graph3 (reduce connect graph2 (rp/edges node))
        graph4 (reduce connect graph3 (remove edge? (rp/successors node)))]
    (reduce connect graph4 (remove edge? (rp/predecessors node)))))

(defn detach
  "removes a node and all its connections from the graph"
  [graph id]
  (let [graph2 (reduce disconnect graph (rp/successors (graph id)))
        graph3 (reduce disconnect graph2 (rp/predecessors (graph2 id)))]
    (dissoc graph3 id)))
