(ns hiposfer.kamal.sqlite
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.spec.test.alpha :refer [instrument]]
            [next.jdbc :as jdbc]))

(def queries-content (slurp "resources/sqlite/queries.sql"))
(def queries (zipmap [:select.way/nodes :select/links
                      :find/destination :find/shortest-path]
                     (map str/trim (str/split queries-content #";\n"))))

(def schema (slurp "resources/sqlite/schema.sql"))

(def protocol "jdbc:sqlite:")

(def filepath "resources/graph.db")
(def uri (str protocol filepath))

(def volatile (str protocol ":memory:"))

(defn setup!
  [conn]
  (doseq [statement (str/split schema #";\n")]
    (jdbc/execute! conn [statement])))


(defn parametrize
  "replaces sqlite named parameters (:NAME) with positional ones. Useful
  in JDBC context where named parameters are not supported"
  [query]
  (str/replace query #":\w+" "?"))
