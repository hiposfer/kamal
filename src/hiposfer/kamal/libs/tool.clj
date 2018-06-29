(ns hiposfer.kamal.libs.tool
  "useful functions that have not found a proper place yet"
  (:refer-clojure :rename {some some*} :exclude [assert])
  (:require [clojure.spec.alpha :as s]))


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

(defn keys?
  "checks that the map m contains the required keys specified in keys-spec.
  Only the keys are checked not the values. Returns nil on error"
  [m keys-spec]
  (let [reqs (apply hash-map (drop 1 (s/form keys-spec)))
        unq  (map keyword (map name (:req-un reqs)))]
    (reduce (fn [_ k] (when (nil? (k m)) (reduced k)))
            nil
            (concat unq (:req reqs)))))

(defn assert
  "checks that m conforms to spec. Returns an error message on error or nil
  otherwise"
  [m spec]
  (when (not (s/valid? spec m))
    (s/explain-str spec m)))

(defn update*
  "takes a map and a mapping of keyword to a 1 argument function. Recursively
   transforms m by updating its value through the passed functions."
  [m mfs]
  (reduce-kv (fn [result k v] (update result k v))
             m
             mfs))

;(keys? {:id 2 :departure 3 :coordinates 4} ::params)
