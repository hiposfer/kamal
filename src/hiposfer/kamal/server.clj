(ns hiposfer.kamal.server
  (:require [ring.util.http-response :refer [ok not-found]]
            [compojure.api.sweet :refer [context GET api ANY undocumented]]
            [compojure.route :as route]
            [hiposfer.kamal.spec :as spec]
            [hiposfer.kamal.directions :as dir]
            [hiposfer.kamal.graph.generators :as g]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))

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

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the grid"
  [grid]
  (api {:swagger {:ui "/"
                  :spec "/swagger.json"
                  :data {:info {:title "Routing API"
                                :description "Routing for hippos"}
                         :tags [{:name "direction", :description "direction similar to mabbox"}]}}
        :api {:invalid-routes-fn compojure.api.routes/fail-on-invalid-child-routes}}
    (GET "/directions/v5/:coordinates" []
      :coercion :spec
      :summary "directions with clojure.spec"
      :path-params [coordinates :- ::spec/raw-coordinates]
      :query-params [{steps :- boolean? false}
                     {radiuses :- ::spec/raw-radiuses nil}
                     {alternatives :- boolean? false}
                     {language :- string? "en"}]
      :return ::spec/direction
      (ok (let [coordinates (parse-coordinates coordinates)
                radiuses    (some-> radiuses (parse-radiuses))]
            (cond
              (not (s/valid? :hiposfer.geojson.specs.linestring/coordinates coordinates))
              {:message (format "Coordinate is invalid: %s"
                          (first (:clojure.spec.alpha/value 
                                  (s/explain-data :hiposfer.geojson.specs.linestring/coordinates coordinates))))
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
                             :language language)))))
    ;; if nothing else matched, return a 404 - not found
    (undocumented
      (route/not-found (not-found "we couldnt find what you were looking for")))))

;(:app hiposfer.kamal.dev/system)