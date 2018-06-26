(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.api.sweet :as sweet]
            [compojure.route :as route]
            [hiposfer.kamal.specs.directions :as dataspecs]
            [hiposfer.kamal.services.routing.directions :as dir]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.libs.fastq :as fastq]
            [clojure.spec.alpha :as s]
            [datascript.core :as data])
  (:import (java.time LocalDateTime)))

;; we are not really using this so I deactivated it for the moment
;; TODO: bring back this functionality once it makes sense
;(defn- validate
;  "validates that the coordinates and the radiuses conform to the mapbox specification.
;  Returns an http response object if validation fails or nil otherwise."
;  [coordinates radiuses]
;  (when (and (not-empty radiuses) (not= (count radiuses) (count coordinates)))
;    (code/unprocessable-entity
;      {:code "InvalidInput"
;       :message "The same amount of radiuses and coordinates must be provided"})))

(def max-distance 1000) ;; meters

(defn- select
  "returns a network whose bounding box contains all points"
  [conns area points]
  (when-let [conn (first conns)]
    (when (some? (data/entity @conn [:area/id area]))
      (let [nodes (map #(:node/location (first (fastq/nearest-node @conn %))) points)
            distances (map geometry/haversine points nodes)]
        (when (and (every? #(not (nil? %)) nodes)
                   (every? #(< % max-distance) distances))
            @conn)))))

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  [router]
  (sweet/api {:swagger {:ui "/"
                        :spec "/swagger.json"
                        :data {:info {:title "kamal"
                                      :description "Routing for hippos"}}}
              :api {:invalid-routes-fn compojure.api.routes/fail-on-invalid-child-routes}}
    (sweet/POST "area/:id/directions" []
      :coercion :spec
      :summary "directions with clojure.spec"
      :path-params [id :- string?]
      :query-params [coordinates :- :hiposfer.geojson.specs.multipoint/coordinates
                     departure   :- ::dataspecs/departure]
      :return ::mapbox/response
      ;(validate (:coordinates arguments) (:radiuses arguments))
      (let [regions @(:networks router)]
        (if (empty? regions) (code/service-unavailable)
          (if-let [network (select regions id coordinates)]
            (code/ok (dir/direction network {:coordinates coordinates
                                             :departure departure}))
            (code/ok {:code "NoSegment"
                      :message "No road segment could be matched for coordinates"})))))
    (sweet/undocumented ;; returns a 404 when nothing matched
      (route/not-found (code/not-found "we couldnt find what you were looking for")))))

;; TESTS
;(fastq/nearest-node @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                    [6.905707,49.398459])

;(time
;  (dir/direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                 {:coordinates [[8.645333, 50.087314]
;                                [8.635897, 50.104172]]
;                               :departure (LocalDateTime/parse "2018-05-07T10:15:30")
;                  :steps true}))
