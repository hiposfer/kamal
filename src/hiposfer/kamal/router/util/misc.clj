(ns hiposfer.kamal.router.util.misc
  "useful functions that have not found a proper place yet"
  (:refer-clojure :rename {some some*} :exclude [assert])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn unique-by
  "Returns a lazy sequence of the elements of coll with duplicates attributes removed.
   Returns a stateful transducer when no collection is provided."
  ([attr coll]
   (eduction (unique-by attr) coll))
  ([pred]
   (let [seen (volatile! (transient #{}))
         rf   (fn [v]
                (if (contains? @seen (pred v))
                  true
                  (do (vswap! seen conj! (pred v))
                      false)))]
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

(defn assert
  "checks that m conforms to spec. Returns an error message on error or nil
  otherwise"
  [m spec]
  (when (not (s/valid? spec m))
    (s/explain-str spec m)))

(defn coerce
  "takes a map and a mapping of keyword to a 1 argument function. Recursively
   transforms m by updating its value through the passed functions. Non existent
   values are ignored"
  [m coercers]
  (reduce-kv (fn [result k v] (if (not (contains? m k)) result
                                (update result k v)))
             m
             coercers))

(defn- stringify?
  "only stringifies java object which are not primities (java.lang...)"
  [^Object value]
  (and (some? value)
       (str/starts-with? (.getCanonicalName (.getClass ^Object value)) "java")
       (not (str/starts-with? (.getCanonicalName (.getClass ^Object value)) "java.lang"))))

(defn jsonista
  "returns a string representation of Java objects or the object itself otherwise"
  [value]
  (cond
    (stringify? value)
    (str value)

    (qualified-keyword? value)
    (keyword (name value))

    :else value))

(defn some
  "an alternative version of Clojure's some which uses reduce instead of
  recur. Useful for collections that know how to reduce themselves faster
  than first/next

  Returns the value that caused (pred? value) to be true; as opposed to Clojure's"
  [pred? coll]
  (reduce (fn [_ value] (when (pred? value) (reduced value)))
          nil
          coll))
