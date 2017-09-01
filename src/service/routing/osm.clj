(ns service.routing.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as imap]
            [service.routing.graph.core :as route]
            [service.routing.graph.protocols :as rp]
            [clojure.walk :as walk]))

;; routing profile taken from
;; http://wiki.openstreetmap.org/wiki/OSM_tags_for_routing/Telenav
(def pedestrian
  {:highways #{"trunk"     "trunk_link"     "primary"     "primary_link"
               "secondary" "secondary_link" "residential"
               "residential_link"           "service"     "tertiary"
               "tertiary_link"              "road"       "track"        "unclassified"
               "undefined" "unknown"        "living_street"             "private"
               "footway"   "pedestrian"     "steps"}
               ;;route=ferry TODO: how should I use this?
   ;;TODO: include routing attributes for penalties
   ;; bridge=yes      Also true/1/viaduct
   ;; tunnel=yes      Also true/1
   ;; surface=paved   No Speed Penalty. Also cobblestone/asphalt/concrete
   ;; surface=unpaved Speed Penalty. Also dirt/grass/mud/earth/sand
   ;; surface=*       Speed Penalty (all other values)
   ;; access=private
   :attrs #{"name"}}) ;:name_1 :ref}})


;; name=*   Street Name (Official). Also official_name / int_name / name:en / nat_name
;; name_1=* Street Name (Alternate). Also reg_name / loc_name / old_name
;; ref=*    Route network and number, but information from parent Route Relations has priority, see below. Also int_ref=* / nat_ref=* / reg_ref=* / loc_ref=* / old_ref=*

;; <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
;;      uid="46882" visible="true" version="1" changeset="676636"
;;      timestamp="2008-09-21T21:37:45Z"/>)
(defn- ->point-entry
  "takes an OSM node and returns a [id Node-instance]"
  [element] ; returns [id node] for later use in int-map
  [(Long/parseLong (:id  (:attrs element)))
   (route/->Point (Double/parseDouble (:lon (:attrs element)))
                  (Double/parseDouble (:lat (:attrs element))))])

; <way id="26659127" user="Masch" uid="55988" visible="true" version="5" changeset="4142606" timestamp="2010-03-16T11:47:08Z">
;   <nd ref="292403538"/>
;   <nd ref="298884289"/>
;   ...
;   <nd ref="261728686"/>
;   <tag k="highway" v="unclassified"/>
;   <tag k="name" v="Pastower Straße"/>
;  </way>
(defn- highway-tag? [element] (when (= "highway" (:k (:attrs element))) element))

(defn- highway->ways
  "parse a OSM xml-way into a [way-id {attrs}] representing the same way"
  [element] ;; returns '(arc1 arc2 ...)
  (let [attrs (into {} (comp (filter #(= :tag (:tag %)))
                             (map    (fn [el] [(:k (:attrs el))
                                               (:v (:attrs el))])))
                       (:content element))
        nodes (into [] (comp (filter #(= :nd (:tag %)))
                             (map (comp :ref :attrs))
                             (map #(Long/parseLong %)))
                       (:content element))]
    [(Long/parseLong (:id (:attrs element)))
     (assoc attrs ::nodes nodes)]))

(defn- valid-way?
  "returns nil if the passed way does not conform to the necessary conditions"
  [[_ attrs :as way]]
  (when (and (contains? (:highways pedestrian) (get attrs "highway"))
             (or (not= "no" (get attrs "access"))
                 (= (get attrs "foot") "yes")))
    way))

(defn- postprocess
  "return a [id way] pair with all unnecessary attributes removed with the
   exception of ::nodes. Reverse nodes if necessary"
  [[id attrs]]
  (let [new-attrs (walk/keywordize-keys
                    (select-keys attrs (conj (:attrs pedestrian) ::nodes)))]
    (if (= "-1" (get attrs "oneway"))
      [id (update new-attrs ::nodes rseq)]
      [id new-attrs])))

(defn- strip-points
  "returns a sequence of node ids that join the intersections
  of this way. The first and last id are included as well."
  [intersections nodes]
  (let [begin (first nodes)
        end   (last nodes)]
    (for [node nodes
          :when (or (= begin node)
                    (= end   node)
                    (contains? intersections node))]
      node)))

(defn- simplify
  "returns a [way-id [connected-node-ids]], i.e. only the nodes that represent
  the connected nodes in a graph structure"
  [ways]
  (let [point-count   (into (imap/int-map) (frequencies (mapcat ::nodes (vals ways))))
        intersections (into (imap/int-set)
                            (comp (filter (fn [[_ v]] (>= v 2)))
                                  (map first))
                            point-count)]
    (map (fn [[id attr]] [id (strip-points intersections (::nodes attr))])
         ways)))


(defn- connect
  "takes a hashmap of nodes and a hashmap of {way [nodes]} and create
  arcs between each node to form a graph"
  [nodes simple-ways]
  (let [arcs (mapcat (fn g [[way-id nodes]] (map route/->Arc nodes (rest nodes) (repeat way-id))))]
    (transduce arcs ;; this approach only works if Arc implements MapEntry
      (completing
        (fn f [graph arc]
          (-> (update-in graph [(rp/src arc) :arcs] conj arc)
              (update-in       [(rp/dst arc) :arcs] conj (rp/mirror arc)))))
      nodes
      simple-ways)))

;; xml-parse: (element tag attrs & content)
(defn- osm->ways
  "takes an OSM-file and returns an int-map of ways representing the road
   network"
  [filename]
  (with-open [file-rdr (clojure.java.io/reader filename)]
    (let [osm   (:content (xml/parse file-rdr))
          ways  (into (imap/int-map)
                      (comp (filter #(= :way (:tag %)))
                            (filter #(some highway-tag? (:content %)))
                            (map highway->ways)
                            (filter valid-way?)
                            (map postprocess))
                      osm)]
      ways)))

(defn- osm->nodes
  "takes an OSM-file and a set of node ids and returns a
   vector of point instances"
  [filename ids]
  (with-open [file-rdr (clojure.java.io/reader filename)]
    (let [osm   (:content (xml/parse file-rdr))
          nodes (into (imap/int-map)
                      (comp (filter #(= :node (:tag %)))
                            (filter #(contains? ids (Long/parseLong (:id (:attrs %)))))
                            (map ->point-entry))
                      osm)]
      nodes)))

;; There are generally speaking two ways to process an OSM file for routing
; - read all points and ways and transform them in memory
; - read it once, extract the ways and then read it again and extract only the relevant nodes

;; The first option is faster since reading from disk is always very slow
;; The second option uses less memory but takes at least twice as long (assuming reading takes the most time)

(defn osm->network
  "read an OSM file twice: once for ways and another for nodes and transforms
  it into a network of {:graph :ways :points}, such that the graph represent
  only the connected nodes, the points represent the shape of the connection
  and the ways are the metadata associated with the connections"
  [filename]
  (let [ways          (osm->ways filename)
        ;; post procesing ways
        point-ids     (into (imap/int-set) (mapcat ::nodes) (vals ways))
        ;; post-processing nodes
        points&nodes  (osm->nodes filename point-ids)
        simple-ways   (simplify ways)
        node-ids      (into (imap/int-set) (mapcat second) simple-ways)
        points        (into (imap/int-map)
                            (remove #(contains? node-ids (key %)))
                            points&nodes)
        nodes         (into (imap/int-map)
                            (comp (filter #(contains? node-ids (key %)))
                                  (map (fn [[k v]] [k (route/->Node (:lon v) (:lat v) {})])))
                            points&nodes)
        graph         (connect nodes simple-ways)]
    {:ways ways :graph graph :points points}))

(def walking-speed  2.5);; m/s

;(System/gc)
;(def network (time (osm->network "resources/osm/saarland.osm")))
;(take 10 (:graph network))
;(def network nil)


;(count (:points network))
;(count (:graph network))
;(count (:ways network))

;; LEARNINGS ----- resources/osm/saarland.osm
;; way count
;; - without filtering for highways 169241
;; - only highways                   72342
;; - only pedestrian highways        61986

;; node count
;; - without filtering for highways 1119289
;; - only highways                   470230
;; - only connecting highways         73614