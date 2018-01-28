(ns hiposfer.kamal.parsers.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as imap]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.data.avl :as avl]
            [clojure.set :as set]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.libs.tool :as tool]
            [hiposfer.kamal.network.graph.core :as graph]
            [hiposfer.kamal.network.core :as network])
  (:import (org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream)))

(defn- bz2-reader
  "Returns a streaming Reader for the given compressed BZip2
  file. Use within (with-open)."
  [filename]
  (-> (io/input-stream filename)
      (BZip2CompressorInputStream.)
      (io/reader)))

;;TODO: include routing attributes for penalties
;; bridge=yes      Also true/1/viaduct
;; tunnel=yes      Also true/1
;; surface=paved   No Speed Penalty. Also cobblestone/asphalt/concrete
;; surface=unpaved Speed Penalty. Also dirt/grass/mud/earth/sand
;; surface=*       Speed Penalty (all other values)
;; access=private

;; name=*   Street Name (Official). Also official_name / int_name / name:en / nat_name
;; name_1=* Street Name (Alternate). Also reg_name / loc_name / old_name
;; ref=*    Route network and number, but information from parent Route Relations has priority,
;;          see below. Also int_ref=* / nat_ref=* / reg_ref=* / loc_ref=* / old_ref=*
(def way-attrs #{"name"}) ;:name_1 :ref}})

;; <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
;;      uid="46882" visible="true" version="1" changeset="676636"
;;      timestamp="2008-09-21T21:37:45Z"/>)
(defn- point-entry
  "takes an OSM node and returns a [id Node-instance]"
  [element] ; returns [id node] for later use in int-map
  [(Long/parseLong (:id  (:attrs element)))
   (network/->Point (Double/parseDouble (:lon (:attrs element)))
                    (Double/parseDouble (:lat (:attrs element))))])

; <way id="26659127" user="Masch" uid="55988" visible="true" version="5" changeset="4142606"
      ;timestamp="2010-03-16T11:47:08Z">
;   <nd ref="292403538"/>
;   <nd ref="298884289"/>
;   ...
;   <nd ref="261728686"/>
;   <tag k="highway" v="unclassified"/>
;   <tag k="name" v="Pastower Straße"/>
;  </way>
(defn- ways-entry
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

(defn- postprocess
  "return a {id way} pair with all unnecessary attributes removed with the
   exception of ::nodes. Reverse nodes if necessary"
  [[id attrs]]
  (let [new-attrs (walk/keywordize-keys
                    (select-keys attrs (conj way-attrs ::nodes)))]
    (if (= "-1" (get attrs "oneway"))
      {id (update new-attrs ::nodes rseq)}
      {id new-attrs})))

(defn- strip-points
  "returns a sequence of node ids that join the intersections
  of this way. The first and last id are included as well."
  [intersections nodes]
  (concat [(first nodes)]
          (sequence (filter #(contains? intersections %)) nodes)
          [(last nodes)]))

(defn- simplify
  "returns a [way-id [connected-node-ids]], i.e. only the nodes that represent
  the connected nodes in a graph structure.

  Uses the ::nodes of each way and counts which nodes appear more than once"
  [ways]
  (let [point-count   (frequencies (sequence (comp (map val)
                                                   (mapcat ::nodes))
                                             ways))
        intersections (into (imap/int-set) (comp (filter #(>= (second %) 2))
                                                 (map first))
                                           point-count)]
    (for [[id attrs] ways]
      [id (strip-points intersections (::nodes attrs))])))


(defn- entries
  "returns a [id node], {id way} or nil otherwise"
  [xml-entry]
  (case (:tag xml-entry)
    :node (point-entry xml-entry)
    :way  (postprocess (ways-entry xml-entry))
    nil))

;; There are generally speaking two ways to process an OSM file for routing
; - read all points and ways and transform them in memory
; - read it once, extract the ways and then read it again and extract only the relevant nodes

;; The first option is faster since reading from disk is always very slow
;; The second option uses less memory but takes at least twice as long (assuming reading takes the most time)

;; We use the first option since it provides faster server start time. We assume
;; that preprocessing the files was already performed and that only the useful
;; data is part of the OSM file. See README

(defn network
  "read an OSM file and transforms it into a network of {:graph :ways :points},
   such that the graph represent only the connected nodes, the points represent
   the shape of the connection and the ways are the metadata associated with
   the connections"
  [filename] ;; read all elements into memory
  (let [nodes&ways    (with-open [file-rdr (bz2-reader filename)]
                        (into [] (comp (map entries)
                                       (remove nil?))
                                 (:content (xml/parse file-rdr))))
        ;; separate ways from nodes
        ways          (into (imap/int-map) (filter map?) nodes&ways)
        points&nodes  (sequence (filter vector?) nodes&ways)
        ;; post-processing nodes
        simple-ways   (simplify ways)
        edges         (for [[way-id nodes] simple-ways]
                        (map network/->Edge nodes (rest nodes) (repeat way-id)))
        node-ids      (into (imap/int-set) (mapcat second) simple-ways)
        nodes         (into (imap/int-map) (comp (filter #(contains? node-ids (first %)))
                                                 (tool/map-vals network/map->NodeInfo))
                                           points&nodes)
        ;points       (apply dissoc points&nodes (mapcat second simple-ways))
        g             (reduce graph/connect nodes (sequence cat edges))]
    {:ways  ways
     :graph g}))
     ;;TODO enable them later to improve precision
     ;; points are basically nodes (in OSM) that are not necessary for routing but that
     ;; describe the shape of a way
     ;;:points points

(def walking-speed  2.5);; m/s

(defn complete
  "computes and assocs the neighbours (to allow nearest neighbour search) and
   bbox (to allow containment testing) into m"
  [m]
  (let [neighbours (into (avl/sorted-map-by geometry/geohash)
                         (set/map-invert (:graph m)))
        bbox       (geometry/bbox (vals (:graph m)))]
    (assoc m :neighbours neighbours
                   :bbox bbox)))

;(System/gc)
;(def network (time (osm->network "resources/osm/saarland.min.osm.bz2")))
;(take 10 (:network network))
;(take 10 (:ways network))
;(def network nil)


;(count (:points network))
;(count (:network network))
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
