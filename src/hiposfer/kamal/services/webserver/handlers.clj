(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.api.sweet :as sweet]
            [compojure.route :as route]
            [hiposfer.kamal.specs.mapbox.directions :as mapbox]
            [hiposfer.kamal.services.routing.directions :as dir]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))

(defn- parse-coordinates
  [text]
  (for [coords (str/split text #";")]
    (mapv edn/read-string (str/split coords #","))))
;(->coordinates "-122.42,37.78;-77.03,38.91")

(defn- parse-radiuses [text] (map edn/read-string (str/split text #";")))
;(->radiuses "1200.50;100;500;unlimited;100")

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  [router]
  (sweet/api {:swagger {:ui "/"}
                  :spec "/swagger.json"
                  :data {:info {:title "Routing API"
                                :description "Routing for hippos"}
                         :tags [{:name "direction", :description "direction similar to mabbox"}]}}
        :api {:invalid-routes-fn compojure.api.routes/fail-on-invalid-child-routes}
    (sweet/GET "/directions/v5/:coordinates" []
      :coercion :spec
      :summary "directions with clojure.spec"
      :path-params [coordinates :- ::mapbox/raw-coordinates]
      :query-params [{steps :- boolean? false}
                     {radiuses :- ::mapbox/raw-radiuses nil}
                     {alternatives :- boolean? false}
                     {language :- string? "en"}]
      :return ::mapbox/direction ;; TODO
      (if (nil? @(:network router)) (code/service-unavailable)
        (let [coordinates (parse-coordinates coordinates)
              radiuses    (some-> radiuses (parse-radiuses))]
          (cond
            (not (s/valid? :hiposfer.geojson.specs.linestring/coordinates coordinates))
            (code/unprocessable-entity
              {:code    "InvalidInput"
               :message (s/explain-data :hiposfer.geojson.specs.linestring/coordinates coordinates)})

            (and (not-empty radiuses) (not= (count radiuses) (count coordinates)))
            (code/unprocessable-entity
              {:code "InvalidInput"
               :message "The same amount of radiouses and coordinates must be provided"})

            :else
            (code/ok (dir/direction @(:network router)
                       :coordinates coordinates
                       :steps steps
                       :radiuses radiuses
                       :alternatives alternatives
                       :language language))))))
    ;; if nothing else matched, return a 404 - not found
    (sweet/undocumented
      (route/not-found (code/not-found "we couldnt find what you were looking for")))))

;(:app hiposfer.kamal.dev/system)
