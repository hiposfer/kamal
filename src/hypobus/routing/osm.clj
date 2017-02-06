(ns hypobus.routing.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as i]
            [clojure.data.int-map :as imap]
            [frechet-dist.protocols :as frepos]
            [hypobus.routing.core :as route]))

(comment certainly some work could be improved using clojure reducers
         but does it worth it?)

;; <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
;;      uid="46882" visible="true" version="1" changeset="676636"
;;      timestamp="2008-09-21T21:37:45Z"/>)
(defn node-tag? [element] (= :node (:tag element)))

(defn element->node-entry
  "parse a OSM xml-node into a Hypobus Node"
  [element] ; returns [id node] for later use in int-map
  (imap/int-map (Long/parseLong (:id  (:attrs element)))
    (route/->Node (Double/parseDouble (:lon (:attrs element)))
                  (Double/parseDouble (:lat (:attrs element)))
                  (imap/int-map)
                  (imap/int-map))))

; <way id="26659127" user="Masch" uid="55988" visible="true" version="5" changeset="4142606" timestamp="2010-03-16T11:47:08Z">
;   <nd ref="292403538"/>
;   <nd ref="298884289"/>
;   ...
;   <nd ref="261728686"/>
;   <tag k="highway" v="unclassified"/>
;   <tag k="name" v="Pastower Straße"/>
;  </way>
(defn highway-tag? [element] (when (= "highway" (:k (:attrs element))) element))
(defn highway-type [element] (keyword (str *ns*) (:v (:attrs element))))
(defn way-tag? [element] (= :way (:tag element)))

(defn highway? [element] (and (way-tag? element) (some highway-tag? (:content element))))

;; TODO: there is definitely more work to do processing ways
(defn highway->arcs
  "parse a OSM xml-way into a vector of Arcs representing the same way"
  [element] ;; returns '(edge1 edge2 ...)
  (let [nodes    (into [] (comp (map #(:ref (:attrs %)))
                                (remove nil?)
                                (map #(Long/parseLong %)))
                       (:content element))
        kind     (highway-type (some highway-tag? (:content element)))
        last-ref (volatile! (first nodes))]
    (into [] (map (fn [ref] (route/->Arc @last-ref (vreset! last-ref ref) -1 kind)))
             (rest nodes))))

(defn upnodes!
  "updates arc with the length between its nodes and, associates arc
  into graph (transient)"
  [graph arc]
  (let [src (get graph (:src-id arc))
        dst (get graph (:dst-id arc))
        ned (assoc arc :length (frepos/distance src dst))]
    (-> graph (assoc! (:src-id arc) (assoc-in src [:out-arcs (:dst-id arc)] ned))
              (assoc! (:dst-id arc) (assoc-in dst [:in-arcs (:src-id arc)] ned)))))

;; xml-parse: (element tag attrs & content)
(defn osm->graph
  "takes an OSM-file and returns an int-map of Nodes representing the road
   network"
  [filename]
  (with-open [file-rdr (clojure.java.io/reader filename)]
    (let [elements   (xml-seq (xml/parse file-rdr))
          nodes&ways (sequence (comp (map #(cond (node-tag? %) (element->node-entry %)
                                                 (highway? %) (highway->arcs %)
                                                 :else nil))
                                     (remove nil?))
                           elements)
          arcs  (sequence (comp (filter vector?) (mapcat identity)) nodes&ways)
          nodes  (into (imap/int-map) (filter map?) nodes&ways)]
      (persistent! (reduce upnodes! (transient nodes) arcs)))))

(defn- unreachable
  "returns a node's id whenever a node doesnt have any out nor in arcs,
  nil otherwise"
  [[id node]]
  (when (and (empty? (:out-arcs node)) (empty? (:in-arcs node)))
    id))

;; TODO: it should check connected components but that is too complicated for directed graphs
(defn cleanup
  "disassociate every node from graph that is unreachable"
  [graph]
  (let [removable (sequence (comp (map unreachable) (remove nil?)) graph)]
    (persistent! (reduce dissoc! (transient graph) removable))))

(def foo (future (time (cleanup (osm->graph "resources/osm/saarland.osm")))))
