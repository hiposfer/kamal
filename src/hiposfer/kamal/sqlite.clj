(ns hiposfer.kamal.sqlite
  (:gen-class)
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io IOException)))

(def schema (slurp "resources/sqlite/schema.sql"))

;(def db-uri "jdbc:sqlite::memory:")

(def graph-file "resources/graph.db")
(def graph-uri (str "jdbc:sqlite:" graph-file))

(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  []
  (try (io/delete-file "resources/graph.db")
       (catch IOException e))
  (let [conn (jdbc/get-connection {:connection-uri graph-uri})
        db   {:connection conn}]
    (println schema)
    ;; somehow I have to split the statements for this to work :/
    (println (jdbc/db-do-commands db (str/split schema #";\n")))
    ;(println (jdbc/insert! db :users {:id 1 :name "Ivan"}))
    ;(println (jdbc/get-by-id db :users 1)) ;; {:id 1 :name "Ivan"}
    ;(println (jdbc/find-by-keys db :users {:name "Ivan"})) ;; ({:id 1 :name "Ivan"})
    ;; on stop
    ;;
    ;; TODO: execute in a terminal
    ;; .open graph-file
    ;; .dump
    (. conn (close))))

;;(-main)
