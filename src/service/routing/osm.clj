(ns service.routing.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as imap]
            [service.routing.graph.core :as route]
            [service.routing.utils.math :as math]))

;; <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
;;      uid="46882" visible="true" version="1" changeset="676636"
;;      timestamp="2008-09-21T21:37:45Z"/>)
(defn- element->node-entry
  "parse a OSM xml-node into a Hypobus Node"
  [element] ; returns [id node] for later use in int-map
  (imap/int-map (Long/parseLong (:id  (:attrs element)))
                (route/->Node (Double/parseDouble (:lon (:attrs element)))
                              (Double/parseDouble (:lat (:attrs element)))
                              nil
                              nil)))

; <way id="26659127" user="Masch" uid="55988" visible="true" version="5" changeset="4142606" timestamp="2010-03-16T11:47:08Z">
;   <nd ref="292403538"/>
;   <nd ref="298884289"/>
;   ...
;   <nd ref="261728686"/>
;   <tag k="highway" v="unclassified"/>
;   <tag k="name" v="Pastower Straße"/>
;  </way>
(defn- highway-tag? [element] (when (= "highway" (:k (:attrs element))) element))
(defn- highway-type [element] (keyword (str *ns*) (:v (:attrs element))))

;; TODO: there is definitely more work to do processing ways
(defn- highway->arcs
  "parse a OSM xml-way into a vector of Arcs representing the same way"
  [element] ;; returns '(arc1 arc2 ...)
  (let [nodes    (sequence (comp (map #(:ref (:attrs %)))
                                 (remove nil?)
                                 (map #(Long/parseLong %)))
                           (:content element))
        kind     (highway-type (some highway-tag? (:content element)))]
    (into [] (map (fn [src dst] (route/->Arc src dst -1 kind))
                  nodes
                  (rest nodes)))))

(defn- upnodes!
  "updates arc with the length between its nodes and, associates arc
  into graph (transient)"
  [graph arc]
  (let [src (get graph (:src arc))
        dst (get graph (:dst arc))
        arc2 (assoc arc :length (math/haversine (:lon src) (:lat src)
                                               (:lon dst) (:lat dst)))]
    (-> graph (assoc! (:src arc2) (assoc-in src [:out-arcs (:dst arc2)]
                                           arc2))
              (assoc! (:dst arc2) (assoc-in dst [:in-arcs (:src arc2)]
                                               arc2)))))

(defn- ->element
  "create a node or a sequence of arcs based on the OSM tag"
  [object]
  (case (:tag object)
    :node (element->node-entry object)
    :way  (when (some highway-tag? (:content object)) (highway->arcs object))
    nil))

;; xml-parse: (element tag attrs & content)
(defn osm->graph
  "takes an OSM-file and returns an int-map of Nodes representing the road
   network"
  [filename]
  (with-open [file-rdr (clojure.java.io/reader filename)]
    (let [elements   (xml-seq (xml/parse file-rdr))
          nodes&ways (into [] (comp (map ->element) (remove nil?))
                              elements)
          arcs       (into [] (comp (filter vector?) cat) nodes&ways)
          nodes      (apply imap/merge (filter map? nodes&ways))]
      (persistent! (reduce upnodes! (transient nodes) arcs)))))

;; in m/s
(def speeds
  "Mapping of OSM-highway type to speed trying to follow both
  http://wiki.openstreetmap.org/wiki/Map_Features#Highway and
  http://wiki.openstreetmap.org/wiki/OSM_tags_for_routing/Maxspeed as close as
  possible"
  {::motorway 30.56,      ::trunk 30.56,        ::primary 19.44,      ::secondary 16.67
   ::tertiary 13.89,       ::motorway_link 13.89, ::trunk_link 13.89,   ::primary_link 13.89
   ::secondary_link 13.89, ::road 11.11,          ::unclassified 11.11, ::residential 8.33
   ::unsurfaced 8.33,     ::living_street 2.78, ::service 1.39})

(def min-speed 0.28) ;;m/s

;(def graph (time (alg/biggest-component (time (osm->graph "resources/osm/saarland.osm")))))
;(def graph (time (osm->graph "resources/osm/saarland.osm")))
;(def graph nil)

;(sequence cat [[1 2] [ 3 4]])
