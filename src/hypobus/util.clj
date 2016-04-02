(ns hypobus.util)

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

(def all-pairs (partial combinations 2))

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

(defn general-trust
  "calculate the average trust in a geo-curve"
  [geo-curve]
  (->> (map :distrust geo-curve)
       (map #(- 1 %))
       (reduce +)
       (#(/ % (count geo-curve)))))

(def last-index
  "returns the last index of a collection"
  (comp dec count))

(defn time-diff
  "calculate the time difference between two instant java.util.Date objects.
  inst-2 is substracted from inst-1. A positive return value means that inst-2
  is after inst-1 whereas a negative value means the opposite. out specifies
  the format for the return value. Valid out values are: :mili, :seconds,
  :minutes, :hours, :days"
  [out ^java.util.Date inst-1 ^java.util.Date inst-2]
  (let [mili-diff (- (.getTime inst-2) (.getTime inst-1))]
    (condp = out
      :mili     mili-diff
      :seconds  (/ mili-diff 1000)
      :minutes  (/ mili-diff (* 60 1000))
      :hours    (/ mili-diff (* 60 60 1000))
      :days     (/ mili-diff (* 24 60 60 1000)))))

(defn pbatch
  "split colls in equal-sized batches and applies f to them in parallel (using pmap).
  Returns all results concatenated. Only useful for computationally intensive
  functions where the time of f dominates the coordination overhead.
  Note: the function will only be applied in parallel if the number of available
  cores is greater than the number of batches, otherwise (f coll) is called"
  [f coll]
  (let [n-cores   (.availableProcessors (Runtime/getRuntime))
        length    (count coll)
        n-batches (int (/ length n-cores))
        batches   (partition-all n-batches coll)]
    (if (> n-cores length)
      (f coll)
      (apply concat (pmap f batches)))))
