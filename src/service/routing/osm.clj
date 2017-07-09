(ns service.routing.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as imap]
            [service.routing.graph.core :as route]
            [service.routing.utils.math :as math]
            [service.routing.graph.algorithms :as alg]))

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
  [element] ;; returns '(edge1 edge2 ...)
  (let [nodes    (sequence (comp (map #(:ref (:attrs %)))
                                 (remove nil?)
                                 (map #(Long/parseLong %)))
                           (:content element))
        kind     (highway-type (some highway-tag? (:content element)))]
    (into [] (map (fn [src dst] (route/->Arc src dst -1 kind)) nodes (rest nodes)))))

(defn- upnodes!
  "updates arc with the length between its nodes and, associates arc
  into graph (transient)"
  [graph arc]
  (let [src (get graph (:src arc))
        dst (get graph (:dst arc))
        ned (assoc arc :length (math/haversine (:lon src) (:lat src)
                                               (:lon dst) (:lat dst)))
        nout (conj (:out-arcs src) ned)
        nin  (conj (:in-arcs dst) ned)]
    (-> graph (assoc! (:src arc) (assoc src :out-arcs nout))
              (assoc! (:dst arc) (assoc dst :in-arcs nin)))))

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


;(def graph (time (alg/biggest-component (time (osm->graph "resources/osm/saarland.osm")))))
;(def graph (time (osm->graph "resources/osm/saarland.osm")))
;(def graph nil)

;(sequence cat [[1 2] [ 3 4]])