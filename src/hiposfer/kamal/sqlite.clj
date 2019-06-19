(ns hiposfer.kamal.sqlite
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.spec.test.alpha :refer [instrument]]))

(def queries-content (slurp "resources/sqlite/queries.sql"))
(def queries (zipmap [:select.way/nodes :select/links
                      :find/destination :find/shortest-path]
                     (map str/trim (str/split queries-content #";\n"))))

(def schema (slurp "resources/sqlite/schema.sql"))

(def protocol "jdbc:sqlite:")

(def filepath "resources/graph.db")
(def uri (str protocol filepath))

(def volatile-graph (str protocol ":memory:"))
