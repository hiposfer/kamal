(ns hiposfer.kamal.server
  (:require [ring.util.http-response :refer [ok]]
            [compojure.api.sweet :refer [context GET api]]
            [hiposfer.kamal.spec :as spec]
            [hiposfer.kamal.directions :as dir]
            [hiposfer.kamal.graph.generators :as g]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn- longitude-in-range?
  [lon]
  (and (<= lon 180) (>= lon -180)))

(defn- latitude-in-range?
  [lat]
  (and (<= lat 90) (>= lat -90)))

(defn- coord-in-range?
  [lon lat]
  (and (longitude-in-range? lon) (latitude-in-range? lat)))

(defn- parse-coordinates
  [text]
  (let [pairs (str/split text #";")]
    (for [coords pairs]
      (mapv edn/read-string (str/split coords #",")))))
;(->coordinates "-122.42,37.78;-77.03,38.91")

(defn- parse-radiuses
  [text]
  (map edn/read-string (str/split text #";")))
;(->radiuses "1200.50;100;500;unlimited;100")


(defn create
  "creates an API handler with a closure around the grid"
  [grid]
  (api {:swagger {:ui "/"
                  :spec "/swagger.json"
                  :data {:info {:title "Routing API"
                                :description "Routing for hippos"}
                         :tags [{:name "direction", :description "direction similar to mabbox"}]}}}
    (GET "/spec/direction/:coordinates" []
      :coercion :spec
      :summary "direction with clojure.spec"
      :path-params [coordinates :- ::spec/raw-coordinates]
      :query-params [{steps :- boolean? false}
                     {radiuses :- ::spec/raw-radiuses nil}
                     {alternatives :- boolean? false}
                     {language :- string? "en"}]
      :return ::spec/direction
      (ok (let [coordinates (parse-coordinates coordinates)
                radiuses    (some-> radiuses (parse-radiuses))
                out-of-range-coord (first (filter #(not(apply coord-in-range? %)) coordinates))]
            (cond
              out-of-range-coord
              {:message (format "Coordinate is invalid: %s" out-of-range-coord)
               :code "InvalidInput"}
              (and (not-empty radiuses) (not= (count radiuses) (count coordinates)))
              {:message "The same amount of radiouses and coordinates must be provided"
               :code "InvalidInput"}
              :else
              (dir/direction @(:network grid)
                             :coordinates coordinates
                             :steps steps
                             :radiuses radiuses
                             :alternatives alternatives
                             :language language)))))))