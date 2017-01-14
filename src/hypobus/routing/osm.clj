(ns hypobus.routing.osm
  (:require [clojure.data.xml :as xml]
            [clojure.data.int-map :as i]
            [clojure.data.int-map :as imap]
            [frechet-dist.protocols :as frepos]))

;; in km/h
(def speeds {::motorway 110,      ::trunk 110,        ::primary 70,      ::secondary 60
             ::tertiary 50,       ::motorway_link 50, ::trunk_link 50,   ::primary_link 50
             ::secondary_link 50, ::road 40,          ::unclassified 40, ::residential 30
             ::unsurfaced 30,     ::living_street 10, ::service 5})

; node-map is a {node-id node}
; node-info is a {:lon :lat :out-edges '(edges) :in-edges '(edge)}
; edge is either Edge or UndirectedEdge
(defrecord Node [^double lon ^double lat out-edges in-edges])
(defrecord Edge [^long src-id ^long dst-id ^double length kind])

(defn node? [x] (instance? hypobus.routing.osm.Node x))

;; <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
;;      uid="46882" visible="true" version="1" changeset="676636"
;;      timestamp="2008-09-21T21:37:45Z"/>)
(defn node-tag? [element] (= :node (:tag element)))

(defn element->node-entry
  [element] ; returns [id node] for later use in int-map
  (imap/int-map (Long/parseLong (:id  (:attrs element)))
                (->Node (Double/parseDouble (:lon (:attrs element)))
                        (Double/parseDouble (:lat (:attrs element)))
                        '()
                        '())))

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

(defn highway?
  [element]
  (and (way-tag? element) (some highway-tag? (:content element))))

(defn highway->edges
  [element] ;; returns '(edge1 edge2 ...)
  (let [nodes    (into [] (comp (map #(:ref (:attrs %)))
                                (remove nil?)
                                (map #(Long/parseLong %)))
                       (:content element))
        kind     (highway-type (some highway-tag? (:content element)))
        last-ref (volatile! (first nodes))]
    (into [] (map (fn [ref] (->Edge @last-ref (vreset! last-ref ref) -1 kind)))
          (rest nodes))))

(defn upnodes
  [graph edge]
  (let [src (get graph (:src-id edge))
        dst (get graph (:dst-id edge))
        ned (assoc edge :length (frepos/distance src dst))]
    (-> graph (assoc! (:src-id edge) (update src :out-edges conj ned))
              (assoc! (:dst-id edge) (update dst :in-edges conj ned)))))

;; xml-parse: (element tag attrs & content)
(defn osm->graph
  [filename]
  (with-open [file-rdr (clojure.java.io/reader filename)]
    (let [elements   (xml-seq (xml/parse file-rdr))
          nodes&ways (sequence (comp (map #(cond (node-tag? %) (element->node-entry %)
                                                 (highway? %) (highway->edges %)
                                                 :else nil))
                                     (remove nil?))
                           elements)
          edges  (sequence (comp (filter vector?) (mapcat identity)) nodes&ways)
          nodes  (into (imap/int-map) (filter map?) nodes&ways)]
      (persistent! (reduce upnodes (transient nodes) edges)))))

(defn- unreachable [[id nd]]
  (when (and (empty? (:out-edges nd)) (empty? (:in-edges nd)))
    id))

(defn cleanup
  [graph]
  (let [removable (sequence (comp (map unreachable) (remove nil?)) graph)]
    (persistent! (reduce dissoc! (transient graph) removable))))

;(def foo (time (cleanup (osm->graph "resources/osm/saarland.osm"))))
;(transduce (comp (map second) (map :out-edges) (map count)) + foo)
(System/gc)
