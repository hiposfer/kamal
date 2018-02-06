(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.api.sweet :as sweet]
            [compojure.route :as route]
            [hiposfer.kamal.specs.mapbox.directions :as mapbox]
            [hiposfer.kamal.services.routing.directions :as dir]
            [clojure.string :as str]
            [hiposfer.kamal.libs.geometry :as geometry]))

(defn- validate
  "validates that the coordinates and the radiuses conform to the mapbox specification.
  Returns an http response object if validation fails or nil otherwise."
  [coordinates radiuses]
  (when (and (not-empty radiuses) (not= (count radiuses) (count coordinates)))
    (code/unprocessable-entity
      {:code "InvalidInput"
       :message "The same amount of radiuses and coordinates must be provided"})))

(def available "xform - returns the networks that are available for computation"
  (comp (map val) (map deref) (remove nil?)))

(defn- select
  "returns a network whose bounding box contains all points"
  [networks points]
  (when-let [net (first networks)]
    (if (every? #(geometry/contains? (:bbox net) %) points) net
      (recur (rest networks) points))))

  ;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  [router]
  (sweet/api {:swagger {:ui "/"
                        :spec "/swagger.json"
                        :data {:info {:title "kamal"
                                      :description "Routing for hippos"}}}
              :api {:invalid-routes-fn compojure.api.routes/fail-on-invalid-child-routes}}
    (sweet/POST "/directions/v5" []
      :coercion :spec
      :summary "directions with clojure.spec"
      :body-params [arguments :- ::mapbox/args]
                    ;; TODO: pull filter
      :return ::mapbox/response
      (let [error   (validate (:coordinates arguments) (:radiuses arguments))
            regions (sequence available (:networks router))]
        (if (some? error) error
          (if (empty? regions) (code/service-unavailable)
            (if-let [network (select regions (:coordinates arguments))]
              (code/ok (dir/direction network arguments))
              (code/ok {:code "NoSegment"
                        :message "No road segment could be matched for coordinates.
                                     Check for coordinates too far away from a road."}))))))
    (sweet/undocumented ;; returns a 404 when nothing matched
      (route/not-found (code/not-found "we couldnt find what you were looking for")))))
