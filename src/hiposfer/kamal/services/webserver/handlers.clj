(ns hiposfer.kamal.services.webserver.handlers
  (:require [ring.util.http-response :as code]
            [compojure.core :as api]
            [compojure.route :as route]
            [hiposfer.kamal.specs.directions :as dirspecs]
            [hiposfer.kamal.specs.resources :as resource]
            [hiposfer.kamal.services.routing.directions :as dir]
            [hiposfer.kamal.libs.geometry :as geometry]
            [hiposfer.kamal.libs.fastq :as fastq]
            [datascript.core :as data]
            [clojure.edn :as edn]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.string :as str])
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
      @conn)))

(defn- select-inside?
  "checks that the provided network contains the coordinates inside it"
  [conns params]
  (let [network (select conns params)]
    (when (not (nil? network))
      (let [cords (for [coord (:coordinates params)
                        :let [node (:node/location (first (fastq/nearest-node network coord)))]
                        :when (not (nil? node))
                        :let [dist (geometry/haversine coord node)]
                        :when (< dist max-distance)]
                    coord)]
        (when (not-empty cords)
          network)))))

;; TODO: create a middleware to check that
;; (if (empty? regions)
;              (code/service-unavailable)

;; TODO: create middleware to validate and coerce the request data

(defn- get-area
  [request]
  (let [regions    (:kamal/networks request)
        ids         (for [conn regions]
                      {:id (data/q '[:find ?id . :where [_ :area/id ?id]] @conn)
                       :type :area})]
    (code/ok {:data ids})))

(defn- get-directions
  [request]
  (if-let [missing (tool/keys? (:params request) ::dirspecs/params)]
    (code/bad-request {:missing missing})
    (let [regions    (:kamal/networks request)
          params      (tool/update* (:params request)
                                {:id          str/upper-case
                                         :coordinates edn/read-string
                                         :departure   #(LocalDateTime/parse %)})
          errors      (tool/assert params ::dirspecs/params)]
      (if (not-empty errors)
        (code/bad-request errors)
        (if-let [network (select-inside? regions params)]
          (code/ok (dir/direction network params))
          (code/ok {:code "NoSegment"
                    :message "No road segment could be matched for coordinates"}))))))

(defn- get-resource
  [request]
  (if-let [missing (tool/keys? (:params request) ::resource/params)]
    (code/bad-request {:missing missing})
    (let [regions    (:kamal/networks request)
          params      (tool/update* (:params request) {:id str/upper-case})
          errors      (tool/assert params ::resource/params)]
      (if (not-empty errors) (code/bad-request errors)
        (if-let [network (select regions params)]
          "foo"
          (code/service-unavailable {:code "NoSegment"
                                     :message "No matching area found"}))))))

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  []
  (api/routes
    (api/GET "/area" request get-area)
    (api/GET "/area/:id/directions" request get-directions)
    (api/GET "/area/:id/:type/:name" request get-resource)
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
