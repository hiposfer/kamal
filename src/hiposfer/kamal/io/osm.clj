(ns hiposfer.kamal.io.osm
  (:require [clojure.data.xml :as xml]
            [hiposfer.kamal.router.util.geometry :as geometry]))

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
(def way-attrs #{"name"})

;; <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
;;      uid="46882" visible="true" version="1" changeset="676636"
;;      timestamp="2008-09-21T21:37:45Z"/>)
(defn- node
  "takes an OSM node and returns a [id Node-instance]"
  [element]
  {:node/id  (Long/parseLong (:id  (:attrs element)))
   :node/lon (Double/parseDouble (:lon (:attrs element)))
   :node/lat (Double/parseDouble (:lat (:attrs element)))})

; <way id="26659127" user="Masch" uid="55988" visible="true" version="5" changeset="4142606"
      ;timestamp="2010-03-16T11:47:08Z">
;   <nd ref="292403538"/>
;   <nd ref="298884289"/>
;   ...
;   <nd ref="261728686"/>
;   <tag k="highway" v="unclassified"/>
;   <tag k="name" v="Pastower Straße"/>
;  </way>
(defn- way
  "parse a OSM xml-way into a [way-id {attrs}] representing the same way"
  [element] ;; returns '(arc1 arc2 ...)
  (let [id (Long/parseLong (:id (:attrs element)))]
    (concat [{:way/id id}]
      (for [[index child] (map vector (range) (:content element))]
        (case (:tag child)
          :tag (when (contains? way-attrs (:k (:attrs child)))
                 {:way_tag/key   (:k (:attrs child))
                  :way_tag/value (:v (:attrs child))
                  :way_tag/way   id})
          :nd {:way_node/node     (Long/parseLong (:ref (:attrs child)))
               :way_node/way      id
               :way_node/sequence index}
          nil)))))

;; We assume that preprocessing the files was already performed and that only
;; the useful data is part of the OSM file. See README
(defn transaction!
  "read an OSM file and transforms it into a sequence of datascript transactions"
  [raw-data] ;; read all elements into memory
  (eduction cat (remove nil?)
            (for [xml-entry (:content (xml/parse raw-data))]
              (case (:tag xml-entry)
                :node [(node xml-entry)]
                :way  (way xml-entry)
                nil))))

;; https://www.wikiwand.com/en/Preferred_walking_speed
(def walking-speed  1.4);; m/s

;; LEARNINGS ----- resources/osm/saarland.osm
;; way count
;; - without filtering for highways 169241
;; - only highways                   72342
;; - only pedestrian highways        61986

;; node count
;; - without filtering for highways 1119289
;; - only highways                   470230
;; - only connecting highways         73614

(defn arcs
  "returns a lazy sequence of arc entries that can be directly transacted
  into sql"
  [rows]
  (for [path      (partition-by :way_node/way rows)
        [from to] (map vector path (rest path))]
    (let [distance (geometry/haversine [(:node/lon from) (:node/lat from)]
                                       [(:node/lon to) (:node/lat to)])]
      {:arc/src      (:way_node/node to)
       :arc/dst      (:way_node/node from)
       :arc/distance distance}
      {:arc/src      (:way_node/node from)
       :arc/dst      (:way_node/node to)
       :arc/distance distance})))
