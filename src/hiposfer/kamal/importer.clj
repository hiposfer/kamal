(ns hiposfer.kamal.importer
  (:gen-class)
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :refer [instrument]]
            [expound.alpha :as expound]
            [hiposfer.kamal.sqlite :as sqlite]
            [hiposfer.kamal.router.io.osm :as osm]
            [hiposfer.kamal.router.util.geometry :as geometry])
  (:import (java.io IOException)
           (java.net URL URLEncoder)))

(def expound-printer (expound/custom-printer {:show-valid-values? false
                                              :theme              :figwheel-theme
                                              :print-specs?       false}))

(alter-var-root #'s/*explain-out* (constantly expound-printer))

(instrument)

(def outdir "resources/test/")
(defn- osm-filename [area] (str outdir (:area/id area) ".osm"))

(defn- fetch-osm!
  "read OSM data either from a local cache file or from the overpass api"
  [area]
  (if (.exists (io/file (osm-filename area)))
    (do (println "OK - OSM file found")
        (osm-filename area))
    (let [query (str/replace (slurp (io/resource "overpass-api-query.txt"))
                             "Niederrad"
                             (:area/name area))
          url   (str "http://overpass-api.de/api/interpreter?data="
                     (URLEncoder/encode query "UTF-8"))
          conn  (. ^URL (io/as-url url) (openConnection))]
      (println "no OSM cache file found ... fetching")
      (io/copy (. conn (getContent))
               (io/file (osm-filename area)))
      (println "OK - writing OSM cache file" (osm-filename area))
      (osm-filename area))))
;;(fetch-osm! {:area/id "frankfurt" :area/name "Frankfurt am Main"})

;; TODO: add arc from dst to src
(defn- arcs
  "returns a lazy sequence of arc entries that can be directly transacted
  into sql"
  [rows]
  (for [path      (partition-by :way_node/way rows)
        [from to] (map vector path (rest path))]
    (let [distance (geometry/haversine [(:node/lon from) (:node/lat from)]
                                       [(:node/lon to) (:node/lat to)])]
      {:arc/src      (:way_node/node from)
       :arc/dst      (:way_node/node to)
       :arc/distance distance})))

(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  []
  (try (io/delete-file sqlite/filepath)
       (println (str sqlite/filepath " deleted"))
       (catch IOException e))
  (with-open [conn (jdbc/get-connection sqlite/uri)
              stream (io/input-stream (fetch-osm! {:area/id   "niederrad"
                                                   :area/name "Niederrad"}))]
    ;; execute each statement separately
    (println "creating tables")
    (doseq [statement (str/split sqlite/schema #";\n")]
      (jdbc/execute! conn [statement]))
    (println "importing OSM")
    (doseq [tx (osm/transaction! stream)]
      (sql/insert! conn (namespace (ffirst tx)) tx))
    (println "linking nodes - creating graph")
    (doseq [arc (arcs (jdbc/execute! conn [(:select.way/nodes sqlite/queries)]))]
      (sql/insert! conn "arc" arc))))
    ;; TODO: execute in a terminal
    ;; .open graph-file
    ;; .dump

;(-main)
