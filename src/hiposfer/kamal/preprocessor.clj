(ns hiposfer.kamal.preprocessor
  (:gen-class)
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
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

(s/def ::env (s/and #(pos? (count %))
                     (s/map-of core/area-id? string?)
                     (s/map-of gtfs-area? string?)))

(defn fetch-osm
  [area]
  (let [query     (str/replace (slurp (io/resource "overpass-api-query.txt"))
                               "Niederrad"
                               (:area/name area))
        url       (str "http://overpass-api.de/api/interpreter?data="
                       (URLEncoder/encode query "UTF-8"))
        conn      (. ^URL (io/as-url url) (openConnection))]
    (osm/datomize! (. conn (getContent)))))

(defn- prepare-data
  [area]
  ;; progressively build up the network from the pieces
  (with-open [z (-> (io/input-stream (:area/gtfs area))
                    (ZipInputStream.))]
    (as-> (data/empty-db routing/schema) $
          (time (data/db-with $ (gtfs/datomize! z)))
          (time (data/db-with $ (fetch-osm area)))
          (time (data/db-with $ (fastq/link-stops $)))
          (time (data/db-with $ (fastq/cache-stop-successors $)))
          (time (data/db-with $ [area]))))) ;; add the area as transaction)))

(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  [outdir]
  (assert (some? outdir) "missing output file")
  ;; setup spec instrumentation and expound for better feedback
  (clojure.spec.test.alpha/instrument)
  (alter-var-root #'s/*explain-out* (constantly dev/custom-printer))
  (timbre/info "preprocessing OSM and GTFS files")
  (let [env    (walk/keywordize-keys (into {} (System/getenv)))
        areas  (into {} (filter #(re-matches core/area-regex (name (key %)))) env)]
    (assert (s/valid? ::env areas) (expound/expound-str ::env areas))
    (doseq [area (core/prepare-areas areas)]
      (println "processing area:" (:area/name area))
      (let [db   (time (prepare-data area))
            ;; osm is mandatory, use its filename !!
            path (str outdir (str/replace (str/lower-case (:area/name area))
                                          " " "-")
                             ".edn.gz")]
        (println "writing data to output file")
        (with-open [w (-> (io/output-stream path)
                          (GZIPOutputStream.)
                          (io/writer))]
          (binding [*out* w]
            (pr db)))
        (println "OK -" (:area/name area))))))

;example
;(-main "resources/")
