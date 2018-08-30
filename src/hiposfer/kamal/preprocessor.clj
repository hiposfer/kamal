(ns hiposfer.kamal.preprocessor
  (:gen-class)
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [hiposfer.kamal.services.routing.core :as routing]
            [hiposfer.kamal.core :as core]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [datascript.core :as data]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.parsers.gtfs.core :as gtfs]
            [hiposfer.kamal.parsers.osm :as osm])
  (:import (java.net URLEncoder URL)
           (java.util.zip ZipInputStream GZIPOutputStream)))

(defn gtfs-area? [k] (str/ends-with? (name k) "GTFS"))

(s/def ::env (s/and #(pos? (count %))
                     (s/map-of core/area-id? string?)
                     (s/map-of gtfs-area? string?)))

(defn- prepare-data
  [area]
  (let [human-id (str/replace (:area/id area) "_" " ")
        query    (str/replace (slurp "resources/overpass-api-query.txt")
                              "Niederrad"
                              human-id)
        url      (str "http://overpass-api.de/api/interpreter?data="
                      (URLEncoder/encode query "UTF-8"))
        conn     (. ^URL (io/as-url url) (openConnection))]
    ;; progressively build up the network from the pieces
    (with-open [z (-> (io/input-stream (:area/gtfs area))
                      (ZipInputStream.))]
      (as-> (data/empty-db routing/schema) $
            (data/db-with $ (osm/datomize! (. conn (getContent))))
            (data/db-with $ (gtfs/datomize! z))
            (data/db-with $ (fastq/link-stops $))
            (data/db-with $ (fastq/cache-stop-successors $))
            (data/db-with $ [area]))))) ;; add the area as transaction)))

(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  [outdir]
  (assert (not (nil? outdir)) "missing output file")
  (timbre/info "preprocessing OSM and GTFS files")
  (let [env    (walk/keywordize-keys (into {} (System/getenv)))
        areas  (into {} (filter #(re-matches core/area-regex (name (key %)))) env)]
    (assert (s/valid? ::env areas) (s/explain ::env areas))
    (doseq [area (core/prepare-areas areas)]
      (println "processing area:" (:area/id area))
      (let [db   (prepare-data area)
            ;; osm is mandatory, use its filename !!
            path (str outdir (str/lower-case (:area/id area)) ".edn.gzip")]
        (with-open [w (-> (io/output-stream path)
                          (GZIPOutputStream.)
                          (io/writer))]
          (binding [*out* w]
            (pr db)))))))

;example
;(-main "resources/")
