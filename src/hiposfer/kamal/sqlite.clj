(ns hiposfer.kamal.sqlite
  (:gen-class)
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :refer [instrument]]
            [expound.alpha :as expound]
            [hiposfer.kamal.router.io.osm :as osm])
  (:import (java.io IOException)
           (java.net URL URLEncoder)))

(alter-var-root #'s/*explain-out*
                (constantly
                  (expound/custom-printer {:show-valid-values? false
                                           :theme              :figwheel-theme
                                           :print-specs?       false})))

(instrument)

(def schema (slurp "resources/sqlite/schema.sql"))

;(def db-uri "jdbc:sqlite::memory:")

(def graph-file "resources/graph.db")
(def graph-uri (str "jdbc:sqlite:" graph-file))

(def outdir "resources/test/")
(defn- osm-filename [area] (str outdir (:area/id area) ".osm"))

(defn fetch-osm!
  "read OSM data either from a local cache file or from the overpass api"
  [area]
  (if (.exists (io/file (osm-filename area)))
    (osm-filename area)
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

(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  []
  (try (io/delete-file graph-file)
       (println (str graph-file " deleted"))
       (catch IOException e))
  (with-open [conn (jdbc/get-connection graph-uri)]
    ;; execute each statement separately
    (jdbc/with-transaction [tx conn]
      (doseq [statement (str/split schema #";\n")]
        (jdbc/execute! tx [statement])))
    (println (sql/insert! conn :node {:node/id 1 :node/lat 95 :node/lon 34}))
    (println (sql/find-by-keys conn :node {:id 1})); {:qualifier :node}))
    (with-open [stream (io/input-stream (fetch-osm! {:area/id   "niederrad"
                                                     :area/name "Niederrad"}))]
      (println (take 10 (for [tx (apply concat (osm/transaction! stream))
                              :when (some? tx)
                              :when (= "way" (namespace (ffirst tx)))]
                          tx))))))
    ;; TODO: execute in a terminal
    ;; .open graph-file
    ;; .dump
