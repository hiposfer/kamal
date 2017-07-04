(ns service.utils.tool
  "useful functions that have not found a proper place yet")


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

(def RADIOUS 6372800); radious of the Earth in meters

(defn haversine
  "Compute the great-circle distance between two points on Earth given their
  longitude and latitude in RADIANS. The distance is computed in meters
  by default."
  [lon-1 lat-1 lon-2 lat-2]
  (let [h  (+ (Math/pow (Math/sin (/ (- lat-2 lat-1) 2)) 2)
              (* (Math/pow (Math/sin (/ (- lon-2 lon-1) 2)) 2)
                 (Math/cos lat-2)
                 (Math/cos lat-1)))]
    (* RADIOUS 2 (Math/asin (Math/sqrt h)))))

;; (frepos/distance [-86.67 36.12] [-118.40 33.94])
;=> 2887.2599506071106 km
; distance between paris and san francisco
; (* (frepos/distance [2.33 48.87] [-122.4 37.8]) (/ 3440.069 6372))
; => 4831.502535634215 nauticals miles

