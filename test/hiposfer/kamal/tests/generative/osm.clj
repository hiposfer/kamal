(ns hiposfer.kamal.tests.generative.osm
  "Test that we can provide routing based solely on OSM data"
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hiposfer.kamal.router.core :as router]
            [hiposfer.kamal.importer :as importer]
            [next.jdbc :as jdbc]
            [hiposfer.kamal.sqlite :as sqlite]))

; -------------------------------------------------------------------
; The Dijkstra algorithm is deterministic, therefore for the same src/dst
; combination it should return the same path
; path(src,dst) = path(src, dst)
(deftest deterministic
  (with-open [conn (jdbc/get-connection sqlite/volatile)]
    (sqlite/setup! conn)
    (importer/populate! conn {:area/id   "niederrad"
                              :area/name "Niederrad"})
    (let [ids       (into [] (map :node/id)
                             (jdbc/plan conn ["select * from node"]))
          [src dst] [(rand-nth ids) (rand-nth ids)]
          results   (for [_ (range 100)]
                      (let [beacon   (sqlite/parametrize
                                       (:find/destination sqlite/queries))
                            radious  (first (jdbc/execute! conn [beacon src dst]))
                            dijkstra (sqlite/parametrize
                                       (:find/shortest-path sqlite/queries))]
                        (jdbc/execute! conn [dijkstra src (:cost radious) dst])))]
      (is ;(or (every? nil? results))
          (and (apply = (map :arc/dst results))
               (apply = (map :cost results)))
          "not deterministic behavior"))))

; -------------------------------------------------------------------
; The Dijkstra algorithm cost is monotonic (increasing)
; https://en.wikipedia.org/wiki/Monotonic_function
(deftest monotonic
  (with-open [conn (jdbc/get-connection sqlite/volatile)]
    (sqlite/setup! conn)
    (importer/populate! conn {:area/id   "niederrad"
                              :area/name "Niederrad"})
    (doseq [_ (range 100)
            :let [ids (into [] (map :node/id)
                               (jdbc/plan conn ["select * from node"]))]]
      (let [[src dst] [(rand-nth ids) (rand-nth ids)]
            beacon   (sqlite/parametrize
                       (:find/destination sqlite/queries))
            radious  (first (jdbc/execute! conn [beacon src dst]))
            dijkstra (sqlite/parametrize
                       (:find/shortest-path sqlite/queries))
            result   (jdbc/execute! conn [dijkstra src (:cost radious) dst])]
        (is (or (empty? result)
                (apply >= (map :cost result)))
            "returned path is not monotonic")))))

; -------------------------------------------------------------------
; If the distance of two nodes is 0 and no edge has a 0 cost,
; then the two nodes MUST be the same
; Ddf(P,Q) = 0 if P = Q
(deftest symmetry
  (with-open [conn (jdbc/get-connection sqlite/volatile)]
    (sqlite/setup! conn)
    (importer/populate! conn {:area/id   "niederrad"
                              :area/name "Niederrad"})
    (doseq [_ (range 100)
            :let [ids (into [] (map :node/id)
                               (jdbc/plan conn ["select * from node"]))]]
      (let [[src]    [(rand-nth ids)]
            beacon   (sqlite/parametrize
                       (:find/destination sqlite/queries))
            radious  (first (jdbc/execute! conn [beacon src src]))
            dijkstra (sqlite/parametrize
                       (:find/shortest-path sqlite/queries))
            result   (time (jdbc/execute! conn [dijkstra src (:cost radious) src]))]
        (is (not (empty? result))
            "no path from node to himself found")
        (is (= 1 (count result))
            "more than one node has to be traverse to reach itself")
        (is (= 0 (:cost (first result)))
            "the cost of going from src to src is zero")))))

;; TODO create a test that supports directions based on OSM routing
;(defspec ^:integration directions
;  30; tries -> expensive test
;  (with-open [conn (jdbc/get-connection sqlite/volatile)]
;    (sqlite/setup! conn)
;    (importer/populate! conn {:area/id   "niederrad"
;                              :area/name "Niederrad"})
;    (let [ids (into #{} (map :node/id)
;                    (jdbc/plan conn ["select * from node"]))]
;      (prop/for-all [i (gen/large-integer* {:min 0 :max (Math/ceil (/ ids 2))})]
;        (let [[src dst] [(nth ids i) (nth ids (* 2 i))]
;              beacon   (sqlite/parametrize (:find/destination sqlite/queries))
;              radious  (first (jdbc/execute! conn [beacon src dst]))
;              dijkstra (sqlite/parametrize (:find/shortest-path sqlite/queries))
;              result   (jdbc/execute! conn [dijkstra src (:cost radious) dst])
;              depart   (gen/generate (s/gen ::dataspecs/departure))
;              args     {:coordinates [src dst] :departure depart :steps true}
;              result   (dir/direction conn args)]
;          (if (nil? result)
;            (do (println "no path found")
;                (is (nil? result) "WTH?"))
;            (is (s/valid? ::dataspecs/directions result)
;                (str (expound/expound-str ::dataspecs/directions result)))))))))

#_(clojure.test/run-tests)
