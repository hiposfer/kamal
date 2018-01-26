(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.api.sweet :as sweet]
            [compojure.route :as route]
            [hiposfer.kamal.specs.mapbox.directions :as mapbox]
            [hiposfer.kamal.services.routing.directions :as dir]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [hiposfer.kamal.libs.geometry :as geometry]))

(defn- parse-coordinates
  [text]
  (for [coords (str/split text #";")]
    (mapv edn/read-string (str/split coords #","))))
;(->coordinates "-122.42,37.78;-77.03,38.91")

(defn- parse-radiuses [text] (map edn/read-string (str/split text #";")))
;(->radiuses "1200.50;100;500;unlimited;100")

(defn- validate
  "validates that the coordinates and the radiuses conform to the mapbox specification.
  Returns an http response object if validation fails or nil otherwise."
  [coordinates radiuses]
  (cond
    (not (s/valid? :hiposfer.geojson.specs.linestring/coordinates coordinates))
    (code/unprocessable-entity
      {:code    "InvalidInput"
       :message (s/explain-data :hiposfer.geojson.specs.linestring/coordinates coordinates)})

    (and (not-empty radiuses) (not= (count radiuses) (count coordinates)))
    (code/unprocessable-entity
      {:code "InvalidInput"
       :message "The same amount of radiuses and coordinates must be provided"})
    :else nil))

(def available "xform - returns the networks that are available for computation"
  (comp (map val) (map deref) (remove nil?)))

(defn- select
  "returns a network whose bounding box contains all points"
  [networks points]
  (transduce available
    (completing (fn [_ nw] (when (every? #(geometry/contains? (:bbox nw) %) points)
                              (reduced nw))))
    nil
    networks))

  ;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  [router]
  (sweet/api {:swagger {:ui "/"
                        :spec "/swagger.json"
                        :data {:info {:title "kamal"
                                      :description "Routing for hippos"}}}
              :api {:invalid-routes-fn compojure.api.routes/fail-on-invalid-child-routes}}
    (sweet/GET "/directions/v5/:coordinates" []
      :coercion :spec
      :summary "directions with clojure.spec"
      :path-params [coordinates :- ::mapbox/raw-coordinates]
      :query-params [{steps :- boolean? false}
                     {radiuses :- ::mapbox/raw-radiuses nil}
                     {alternatives :- boolean? false}
                     {language :- string? "en"}]
      :return ::mapbox/direction
      (let [coordinates (parse-coordinates coordinates)
            radiuses    (some-> radiuses (parse-radiuses))
            error       (validate coordinates radiuses)]
          (if (some? error) error
            (if-let [network (select (:networks router) coordinates)]
              (code/ok (dir/direction network
                         :coordinates coordinates
                         :steps steps
                         :radiuses radiuses
                         :alternatives alternatives
                         :language language))
              (if (empty? (sequence available (:networks router)))
                (code/service-unavailable)
                (code/ok {:code "NoSegment"
                          :message "No road segment could be matched for coordinates.
                                       Check for coordinates too far away from a road."}))))))
    (sweet/undocumented ;; returns a 404 when nothing matched
      (route/not-found (code/not-found "we couldnt find what you were looking for")))))
