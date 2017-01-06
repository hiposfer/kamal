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

(defn update-vals
  "takes an associative structure, a vector of keys and a function and returns
  the structure with the values of keys updated by applying f to them. Note that
  reduce-kv is supported on vectors, where the keys will be the ordinals.
  example (update-vals [:a 2 :b 3] str)"
  ([coll f]
   (if (vector? coll) ;; or map?
    (update-vals coll (range (count coll)) f)
    (update-vals coll (keys coll) f)))
  ([coll keys f] (reduce (fn [m k] (update m k f)) coll keys)))

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
