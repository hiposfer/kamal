(ns hypobus.utils.tool
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]))


(defn combinations
 "returns a lazy sequence of all the possible combinations of the elements in
coll in groups of n members. Example: (combinations 2 [:a :b :c])
                                      ;=> ((:a :b) (:a :c) (:b :c))"
  [n coll]
  (if (= 1 n)
    (map list coll)
    (lazy-seq (when-let [[head & tail] (seq coll)]
                (concat (for [x (combinations (dec n) tail)] (cons head x))
                        (combinations n tail))))))

(defn fmap
  "takes a hash-map and a function and returns a hash-map with the same keys
  and with (function value) as each value"
  [f coll]
  (into {} (for [[kname value] coll]
             [kname (f value)])))

(defn update-vals
  "takes a hash-map, a vector of keys and a function and returns the hash-map
  with the values of mkeys updated by applying f to them.
  Example: (update-vals {:a 2 :b 3} [:a :b] str) ;=> {:a '2', :b '3'}"
  [coll mkeys f]
  (reduce (fn [m mk] (update m mk f)) coll mkeys))

(def last-index
  "returns the last index of a collection"
  (comp dec count))

(defn parse-number
  "coerces a string containing either an integer or a floating point number"
  [^String str-number]
  (let [res (edn/read-string str-number)]
    (when (number? res) res)))

(defn read-curve [filename] (json/read-str (slurp filename) :key-fn keyword))

(defn geojson->curve [filename] (-> (read-curve filename) :geometry :coordinates))
