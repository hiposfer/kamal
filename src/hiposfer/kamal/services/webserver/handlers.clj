(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.api.sweet :as sweet]
            [compojure.route :as route]
            [hiposfer.kamal.specs.mapbox.directions :as mapbox]
            [hiposfer.kamal.services.routing.directions :as dir]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.libs.fastq :as fastq]
            [hiposfer.kamal.services.routing.transit :as transit]
            [datascript.core :as data])
  (:import (java.time LocalDateTime)))

(defn- validate
  "validates that the coordinates and the radiuses conform to the mapbox specification.
  Returns an http response object if validation fails or nil otherwise."
  [coordinates radiuses]
  (when (and (not-empty radiuses) (not= (count radiuses) (count coordinates)))
    (code/unprocessable-entity
      {:code "InvalidInput"
       :message "The same amount of radiuses and coordinates must be provided"})))

(def max-distance 1000) ;; meters

(defn- select
  "returns a network whose bounding box contains all points"
  [conns points]
  (when-let [conn  (first conns)]
    (let [nodes (map #(:node/location (first (fastq/nearest-node @conn %))) points)
          distances (map geometry/haversine points nodes)]
      (if (every? #(< % max-distance) distances) @conn
        (recur (rest conns) points)))))

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
            regions @(:networks router)]
        (if (some? error) error
          (if (empty? regions) (code/service-unavailable)
            (if-let [network (select regions (:coordinates arguments))]
              (code/ok (dir/direction network arguments))
              (code/ok {:code "NoSegment"
                        :message "No road segment could be matched for coordinates"}))))))
    (sweet/undocumented ;; returns a 404 when nothing matched
      (route/not-found (code/not-found "we couldnt find what you were looking for")))))


;; TESTS
;{"arguments":
; {"coordinates": [[6.905707,49.398459],
;                  [6.8992, 49.4509]]}}

;(fastq/nearest-node @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                    [6.905707,49.398459])

;(time
;  (dotimes [n 20]
;    (dir/direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                   {;:steps       true
;                    :coordinates [[6.905707, 49.398459]
;                                  [6.8992, 49.4509]]
;                    :departure   (LocalDateTime/now)})))
;
;(time
;  (dir/direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                 {;:steps       true
;                  :coordinates [[6.905707, 49.398459]
;                                [6.8992, 49.4509]]
;                  :departure   (LocalDateTime/now)}))
