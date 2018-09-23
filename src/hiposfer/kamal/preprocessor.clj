(ns hiposfer.kamal.preprocessor
  (:gen-class)
  (:require [clojure.java.io :as io]
            [hiposfer.kamal.dev :as dev]
            [hiposfer.kamal.services.routing.core :as routing]
            [hiposfer.kamal.core :as core]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [datascript.core :as data]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.parsers.gtfs :as gtfs]
            [hiposfer.kamal.parsers.osm :as osm]
            [expound.alpha :as expound])
  (:import (java.net URLEncoder URL)
           (java.util.zip ZipInputStream GZIPOutputStream)))

(defn gtfs-area? [k] (str/ends-with? (name k) "GTFS"))
(defn area-name? [k] (str/ends-with? (name k) "NAME"))

(s/def ::entry (s/or :name area-name? :gtfs gtfs-area?))
(s/def ::env (s/and #(pos? (count %))
                     (s/map-of core/area-entry? string?)
                     (s/map-of ::entry string?)))

;; predefined output directory to avoid committing the files to the history
(def outdir "resources/test/")
(defn- osm-filename [area] (str outdir (:area/id area) ".osm"))

(defn- read-osm
  [area]
  (println "OK - OSM cache file found at" (osm-filename area))
  (with-open [stream (io/input-stream (osm-filename area))]
    (osm/transaction! stream)))

(defn fetch-osm!
  "read OSM data either from a local cache file or from the overpass api"
  [area]
  (if (.exists (io/file (osm-filename area)))
    (read-osm area)
    (let [query     (str/replace (slurp (io/resource "overpass-api-query.txt"))
                                 "Niederrad"
                                 (:area/name area))
          url       (str "http://overpass-api.de/api/interpreter?data="
                         (URLEncoder/encode query "UTF-8"))
          conn      (. ^URL (io/as-url url) (openConnection))]
      (println "no OSM cache file found ... fetching")
      (io/copy (. conn (getContent)) (io/file (osm-filename area)))
      (println "OK - writing OSM cache file" (osm-filename area))
      (read-osm area))))
;;(fetch-osm! {:area/id "frankfurt" :area/name "Frankfurt am Main"})

(defn- prepare-data!
  "reads both OSM and GTFS files, converts them into Datascript transactions
  and returns a Datascript in-memory Database"
  [area]
  ;; progressively build up the network from the pieces
  (with-open [z (-> (io/input-stream (:area/gtfs area))
                    (ZipInputStream.))]
    (as-> (data/empty-db routing/schema) $
          (time (data/db-with $ (fetch-osm! area)))
          (time (data/db-with $ (gtfs/transaction! z)))
          (time (data/db-with $ (fastq/link-stops $)))
          (time (data/db-with $ (fastq/cache-stop-successors $)))
          (time (data/db-with $ [area]))))) ;; add the area as transaction

(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  []
  ;; setup spec instrumentation and expound for better feedback
  (clojure.spec.test.alpha/instrument)
  (alter-var-root #'s/*explain-out* (constantly dev/custom-printer))
  (println "preprocessing OSM and GTFS files")

  (let [env    (walk/keywordize-keys (into {} (System/getenv)))
        areas  (into {} (filter #(re-matches core/area-regex (name (key %)))) env)]
    (assert (s/valid? ::env areas)
            (expound/expound-str ::env areas))
    (doseq [area (core/prepare-areas areas)]
      (println "processing area:" (:area/name area))
      (let [db   (time (prepare-data! area))
            path (str outdir (:area/id area) ".edn.gz")]
        (println "writing data to" path)
        (with-open [w (-> (io/output-stream path)
                          (GZIPOutputStream.)
                          (io/writer))]
          (binding [*out* w]
            (pr db)))
        (println "OK -" (:area/name area))))))

;example
;(-main "resources/")
