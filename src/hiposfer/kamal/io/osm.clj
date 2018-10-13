(ns hiposfer.kamal.io.osm
  (:require [clojure.data.xml :as xml]
            [hiposfer.kamal.network.core :as network]))

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
(defn- point-entry
  "takes an OSM node and returns a [id Node-instance]"
  [element] ; returns [id node] for later use in int-map
  {:node/id  (Long/parseLong (:id  (:attrs element)))
   :node/location (network/->Location (Double/parseDouble (:lon (:attrs element)))
                                      (Double/parseDouble (:lat (:attrs element))))})

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
  (let [attrs (eduction (filter #(= :tag (:tag %)))
                        (filter #(contains? way-attrs (:k (:attrs %))))
                        (map    #(vector (keyword "way" (:k (:attrs %)))
                                         (:v (:attrs %))))
                        (:content element))
        nodes (eduction (filter #(= :nd (:tag %)))
                        (map (comp :ref :attrs))
                        (map #(Long/parseLong %))
                        (:content element))]
    (into {:way/id (Long/parseLong (:id (:attrs element)))
           :way/nodes nodes}
          attrs)))

(defn- join-ways
  "merges ways that are linked to each other by their head or their tail.

  This is an optimization HACK.

  We artificially merge ways because OSM might decide to break a road into
  reusable pieces (ways) which makes the graph representation very redundant.
  This way we reduce both the amount of way entries in Datascript and the
  amount of nodes"
  ([group] (join-ways (rest group) (first group) nil))
  ([ways current result]
   (let [fncurrent (first (:way/nodes current))
         lncurrent (last (:way/nodes current))
         [point match] (some (fn [way]
                               (cond
                                 (= fncurrent (last  (:way/nodes way))) [:start way]
                                 (= lncurrent (first (:way/nodes way))) [:end way]))
                             ways)]
     (cond
       (empty? ways)
       (conj result current)

       (some? point)
       (recur (remove #(= % match) ways)
              (if (= :start point)
                (update match :way/nodes concat (rest (:way/nodes current)))
                (update current :way/nodes concat (rest (:way/nodes match))))
              result)

       :else (recur (rest ways) (first ways) (conj result current))))))

(defn- trim-ways
  "returns the ways sequence including only the first/last and intersection
   nodes i.e. the connected nodes in a graph structure.

  Uses the :way/nodes of each way and counts which nodes appear more than once"
  [ways]
  (let [groups      (group-by :way/name ways)
        point-count (frequencies (mapcat :way/nodes ways))]
    (for [[way-name group] groups
          :let [group-ways (if (empty? way-name) group
                             (join-ways group))]
          way group-ways]
      (assoc way :way/nodes
        (filter #(or (= % (first (:way/nodes way)))
                     (>= (point-count %) 2)
                     (= % (last (:way/nodes way))))
                 (:way/nodes way))))))

(defn- entries
  "returns a [id node], {id way} or nil otherwise"
  [xml-entry]
  (case (:tag xml-entry)
    :node (point-entry xml-entry)
    :way  (ways-entry xml-entry)
    nil))

;; We assume that preprocessing the files was already performed and that only
;; the useful data is part of the OSM file. See README
(defn transaction!
  "read an OSM file and transforms it into a sequence of datascript transactions"
  [raw-data] ;; read all elements into memory
  (let [nodes&ways    (keep entries (:content (xml/parse raw-data)))
        ;; separate ways from nodes
        ways          (trim-ways (filter :way/id nodes&ways))
        ;; post-processing nodes
        ids           (into #{} (mapcat :way/nodes) ways)
        nodes         (filter #(contains? ids (:node/id %)) nodes&ways)
        arcs          (for [way ways
                            [from to] (map vector (:way/nodes way)
                                                  (rest (:way/nodes way)))]
                        {:edge/src [:node/id from]
                         :edge/dst [:node/id to]
                         :edge/way [:way/id (:way/id way)]})]
    (concat nodes
            (map #(dissoc % :way/nodes) ways)
            arcs)))

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
