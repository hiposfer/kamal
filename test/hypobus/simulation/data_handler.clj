(ns hypobus.simulation.data-handler
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.core.reducers :as red]
            [hypobus.utils.tool :as tool]
            [hypobus.basics.geometry :as geo]))

(defn- parse-number
  "coerces a string containing either an integer or a floating point number"
  [^String str-number]
  (let [res (edn/read-string str-number)]
    (when (number? res) res)))

(def ^:cons ^:private csv->hash-map (partial zipmap [:timestamp :line-id :direction
                                                     :journey-pattern-id :time-frame :vehicle-journey-id :operator
                                                     :congestion :lon :lat :delay :block-id :vehicle-id :stop-id :at-stop]))

(defn journey-id
  [{journey-pattern-id :journey-pattern-id}]
  (cond
    (zero? (count journey-pattern-id)) "EMPTY-ID"
    (< (count journey-pattern-id) 4) journey-pattern-id
    :else (let [line-pattern (seq (subs journey-pattern-id 0 4))
                journey-seq  (drop-while #(= \0 %) line-pattern)]
            (apply str journey-seq))))

(defn fetch-data
  ([filename]
   (fetch-data filename nil))
  ([filename xform]
   (with-open [in-file (io/reader filename)]
     (let [raw-data       (csv/read-csv in-file :separator \,)
           vec->hashmap   (map csv->hash-map)
          ;taker          (take 1000)
           remove-null    (remove #(= (:journey-pattern-id %) "null"))
           str->num       (map #(tool/update-vals % [:lat :lon] parse-number))
           processesor    (if xform (comp vec->hashmap xform remove-null str->num)
                            (comp vec->hashmap remove-null str->num))]
       (into [] processesor raw-data)))))

(defn fetch-line [filename line-id] (fetch-data filename (filter #(= (:line-id %) line-id))))
(defn fetch-journeys [filename journeys] (fetch-data filename (filter #((set journeys) (:journey-pattern-id %)))))
(defn fetch-all [filename] (fetch-data filename (map #(select-keys % [:lat :lon :journey-pattern-id :vehicle-journey-id]))))
(defn fetch-parsed-id [filename line-id] (fetch-data filename (filter #(=  (journey-id %) line-id))))

(defn organize-journey
  [data]
  (let [gap-remover    (mapcat #(geo/split-at-gaps geo/haversine %))
        trans-curves   (map #(geo/tidy geo/haversine %))
        remove-fault   (remove #(< (count %) 5))
        prepare-data   (comp gap-remover trans-curves remove-fault)
        raw-trajectories (vals (group-by :vehicle-journey-id data))]
       (into [] prepare-data raw-trajectories)))

;; -------------------- GTFS files related functions -------------------------;
;; -------------------- GTFS files related functions -------------------------;

(defn raw-shapes
  [filename]
  (with-open [in-file (io/reader filename)]
    (let [fielder  (partial zipmap [:id :lat :lon :sequence :dist-traveled])
          raw-data (rest (csv/read-csv in-file :separator \,))]
      (into [] (red/map fielder raw-data)))))

(defn shape-id [id] (second (re-find #"0-(.*?)-" id)))
(defn shapes [filename] (group-by :id (raw-shapes filename)))

(defn geo-names [dpath] (map str (rest (file-seq (clojure.java.io/file dpath)))))
(defn fname->line-id [fname] (second (re-find #".*\/(.*?)\.geojson" fname)))

(defn geocurves
  [dpath]
  (into {}
    (for [fname (geo-names dpath)]
      [(fname->line-id fname)
       (map #(zipmap [:lon :lat] %) (tool/geojson->curve fname))])))

(defn geoshapes [fname]
  (into {}
    (for [[id points] (shapes fname)]
      [(shape-id id) (map #(tool/update-vals % [:lat :lon] parse-number) points)])))

;(def baz (into #{} (map shape-id (keys foo))))
;(def foo (shapes "assets/gtfs/shapes.txt"))
;(diff bar baz)
