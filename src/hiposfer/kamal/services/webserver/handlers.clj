(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.core :as api]
            [compojure.route :as route]
            [hiposfer.kamal.specs.directions :as dirspecs]
            [hiposfer.kamal.services.routing.directions :as dir]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.libs.fastq :as fastq]
            [datascript.core :as data]
            [clojure.edn :as edn]
            [hiposfer.kamal.libs.tool :as tool])
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
  [conns params]
  (when-let [conn (first conns)]
    (if (not (some? (data/entity @conn [:area/id (:id params)])))
      (recur (rest conns) params)
      (let [nodes (map #(:node/location (first (fastq/nearest-node @conn %)))
                        (:coordinates params))
            distances (map geometry/haversine (:coordinates params) nodes)]
        (when (and (every? #(not (nil? %)) nodes)
                   (every? #(< % max-distance) distances))
            @conn)))))

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  [router]
  (api/routes
    (api/GET "/area/:id/directions" request
      (if-let [missing (tool/keys? (:params request) ::dirspecs/params)]
        (code/bad-request {:missing missing})
        (let [regions    @(:networks router)
              params      (tool/update* (:params request)
                                  {:coordinates edn/read-string
                                   :departure #(LocalDateTime/parse %)})
              errors      (tool/assert params ::dirspecs/params)]
          (if (not-empty errors)
            (code/bad-request errors)
            (if (empty? regions)
              (code/service-unavailable)
              (if-let [network (select regions params)]
                (code/ok (dir/direction network params))
                (code/ok {:code "NoSegment"
                          :message "No road segment could be matched for coordinates"})))))))
    (route/not-found
      (code/not-found "we couldnt find what you were looking for"))))

;; TESTS
;(fastq/nearest-node @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                    [6.905707,49.398459])

;(time
;  (dir/direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                 {:coordinates [[8.645333, 50.087314]
;                                [8.635897, 50.104172]]
;                               :departure (LocalDateTime/parse "2018-05-07T10:15:30")
;                  :steps true}))
