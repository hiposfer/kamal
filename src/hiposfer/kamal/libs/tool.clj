(ns hiposfer.kamal.libs.tool
  "useful functions that have not found a proper place yet"
  (:refer-clojure :rename {some some*})
  (:require [datascript.core :as data]))


(defn unique-by
  "Returns a lazy sequence of the elements of coll with duplicates attributes removed.
   Returns a stateful transducer when no collection is provided."
  ([attr coll]
   (sequence (unique-by attr) coll))
  ([attr]
   (let [seen (volatile! (transient {}))
         rf   (fn [v] (if (get @seen (attr v)) true
                        (do (vswap! seen assoc! (attr v) true) false)))]
     (remove rf))))

(defn unique
  "Returns a lazy sequence of the elements of coll with duplicates removed.
   Returns a stateful transducer when no collection is provided."
  ([] (unique-by identity))
  ([coll] (unique-by identity coll)))

(defn combinations
 "returns a lazy sequence of all the possible combinations of the elements in
  coll in groups of n members.
  Example: (combinations 2 [:a :b :c]) ;;=> ((:a :b) (:a :c) (:b :c))"
  [n coll]
  (if (= 1 n)
    (map list coll)
    (lazy-seq (when-let [[head & tail] (seq coll)]
                (concat (for [x (combinations (dec n) tail)] (cons head x))
                        (combinations n tail))))))

(defn map-vals
  "map over values ONLY"
  ([f coll] (map (fn [[k v]] [k (f v)])
                 coll))
  ([f] (map (fn [[k v]] [k (f v)]))))

(defn some
  "an alternative version of Clojure's some which uses reduce instead of
  recur. Useful for collections that know how to reduce themselves faster
  than first/next

  Returns the value that caused (pred? value) to be true; as opposed to Clojure's"
  [pred? coll]
  (reduce (fn [_ value] (when (pred? value) (reduced value)))
          nil
          coll))

(defn node-successors
  "takes a network and an entity and returns the successors of that entity.
   Only valid for OSM nodes. Assumes bidirectional links i.e. nodes with
   back-references to it are also returned"
  [network entity]
  (map #(data/entity network %)
       (concat (map :db/id (:node/successors entity))
               (map :e (take-while #(= (:v %) (:db/id entity))
                         (data/index-range network :node/successors (:db/id entity) nil))))))

(defn nearest-location
  "returns the nearest node/location datom to point"
  [network point]
  (first (data/index-range network :node/location point nil)))

(defn node-ways
  "takes a dereferenced Datascript connection and an entity id and returns
  the OSM ways that reference it. Only valid for OSM node ids"
  [network id]
  (take-while #(= (:v %) id) (data/index-range network :way/nodes id nil)))
