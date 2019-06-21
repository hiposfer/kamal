(ns hiposfer.kamal.tests.unit
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [next.jdbc.sql :as sql]
            [next.jdbc :as jdbc]
            [hiposfer.kamal.sqlite :as sqlite]))

;; Example taken from
;; https://rosettacode.org/wiki/Dijkstra%27s_algorithm
;; we assume bidirectional links
(def rosetta [{:node/id 1 :node/lat 1 :node/lon 1}
              {:node/id 2 :node/lat 1 :node/lon 1}
              {:node/id 3 :node/lat 1 :node/lon 1}
              {:node/id 4 :node/lat 1 :node/lon 1}
              {:node/id 5 :node/lat 1 :node/lon 1}
              {:node/id 6 :node/lat 1 :node/lon 1}
              {:arc/src 1 :arc/dst 2 :arc/distance 7}
              {:arc/src 1 :arc/dst 3 :arc/distance 9}
              {:arc/src 1 :arc/dst 6 :arc/distance 14}
              {:arc/src 2 :arc/dst 3 :arc/distance 10}
              {:arc/src 2 :arc/dst 4 :arc/distance 15}
              {:arc/src 3 :arc/dst 4 :arc/distance 11}
              {:arc/src 3 :arc/dst 6 :arc/distance 2}
              {:arc/src 4 :arc/dst 5 :arc/distance 6}
              {:arc/src 5 :arc/dst 6 :arc/distance 9}])

;; --- directed arcs
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 26) (6 11))
;Shortest path: (1 3 4 5)

;; --- bidirectional arcs ... see kotlin solution
;Distances from 1: ((1 0) (2 7) (3 9) (4 20) (5 20) (6 11))
;Shortest path: (1 3 6 5)

(defn- populate!
  "inserts the data from `rosetta` into sqlite; assuming bidirectional arcs"
  [conn]
  (doseq [entry rosetta
          :when (= "node" (namespace (ffirst entry)))]
    (sql/insert! conn "node" entry))
  (doseq [entry rosetta
          :when (= "arc" (namespace (ffirst entry)))]
    (sql/insert! conn "arc" entry)
    (sql/insert! conn "arc" (assoc entry
                                   :arc/src (:arc/dst entry)
                                   :arc/dst (:arc/src entry)))))

(deftest destination-exists
  (with-open [conn (jdbc/get-connection sqlite/volatile)]
    (sqlite/setup! conn)
    (populate! conn)
    (let [[src dst] [1 5]
          query     (sqlite/parametrize (:find/destination sqlite/queries))
          result    (first (jdbc/execute! conn [query src dst]))]
      (is (not (empty? result)) "destination not found")
      (is (= dst (:arc/dst result)))
      (is (= 20.0  (:cost result))))))

(deftest shortest-path
  (with-open [conn (jdbc/get-connection sqlite/volatile)]
    (sqlite/setup! conn)
    (populate! conn)
    (let [[src dst] [1 5]
          beacon   (sqlite/parametrize (:find/destination sqlite/queries))
          radious  (first (jdbc/execute! conn [beacon src dst]))
          dijkstra (sqlite/parametrize (:find/shortest-path sqlite/queries))
          result   (jdbc/execute! conn [dijkstra src (:cost radious) dst])]
      (is (not (empty? result))
          "shortest path not found")
      (is (= '([5 20.0] [6 11.0] [3 9.0] [1 0])
              (map (juxt :arc/dst :cost) result))
          "shortest path doesnt traverse expected nodes"))))
