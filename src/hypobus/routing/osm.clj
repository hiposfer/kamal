(ns hypobus.routing.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as i]))

(def speeds {:motorway 110,      :trunk 110,        :primary 70,      :secondary 60
             :tertiary 50,       :motorway_link 50, :trunk_link 50,   :primary_link 50
             :secondary_link 50, :road 40,          :unclassified 40, :residential 30
             :unsurfaced 30,     :living_street 10, :service 5})

(defrecord node [^double lat ^double lon])

(defn- pathway?
  [object]
  (and (= :way (:tag object))
       (some #(and (= :tag (:tag %)) (= "highway" (:k (:attrs %))))
             (:content object))))

(defn- entry
  [element]
  (cond
    (= :node (:tag element)) (i/int-map (Long/parseLong (:id  (:attrs element)))
                                        (->node (Double/parseDouble (:lat (:attrs element)))
                                                (Double/parseDouble (:lon (:attrs element)))))
    (pathway? element) element ;;TODO
    :else nil))

#_(let [data (with-open [file-rdr (clojure.java.io/reader "resources/osm/saarland.osm")]
              (into [] (comp (map entry)
                             (remove nil?))
                    (xml-seq (xml/parse file-rdr))))
        nodes (red/fold i/merge (filter map? data))]
   #_(loop [graph  nodes
            edges (filter vector? nodes)]

      nil))


;; (with-open [file-rdr (clojure.java.io/reader "resources/osm/saarland.osm")]
;;              (into [] (comp (map entry)
;;                             (remove nil?)
;;                             (filter record?)
;;                             (take 10))
;;                    (xml-seq (xml/parse file-rdr))))
