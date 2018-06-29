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

(defn preprocess
  "checks that the passed request conforms to spec and coerce its params
  if so. Returns a possibly modified request.

  We do it like this instead of creating a middleware for readability. We
  need access to the path parameters which are only added after the route
  has been matched.

  See: https://groups.google.com/forum/?hl=en#!topic/compojure/o5l9m7nbGlE"
  [request spec coercer]
  (if-let [missing (tool/keys? (:params request) spec)]
    (code/bad-request {:missing missing})
    (let [params      (tool/update* (:params request) coercer)
          errors      (tool/assert params spec)]
      (if (empty? errors)
        (assoc request :params params)
        (assoc request :params params :kamal/errors errors)))))

(defn- get-area
  [request]
  (let [regions    (:kamal/networks request)
        ids        (for [conn regions]
                     {:id (data/q '[:find ?id . :where [_ :area/id ?id]] @conn)
                      :type :area})]
    (code/ok {:data ids})))

(def directions-coercer {:id          str/upper-case
                         :coordinates edn/read-string
                         :departure   #(LocalDateTime/parse %)})

(defn- get-directions
  [request]
  (if (some? (:kamal/errors request))
    (code/bad-request (:kamal/errors request))
    (let [regions (:kamal/networks request)]
      (if-let [network (select-inside? regions (:params request))]
        (code/ok (dir/direction network (:params request)))
        (code/ok {:code "NoSegment"
                  :message "No road segment could be matched for coordinates"})))))

(defn- get-resource
  [request]
  (if (some? (:kamal/errors request))
    (code/bad-request (:kamal/errors request))
    (let [regions (:kamal/networks request)]
      (if-let [network (select regions (:params request))]
        "foo"
        (code/service-unavailable {:code "NoSegment"
                                   :message "No matching area found"})))))

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  []
  (api/routes
    (api/GET "/area" request (get-area request))
    (api/GET "/area/:id/directions" request
      (get-directions (preprocess request ::dirspecs/params directions-coercer)))
    (api/GET "/area/:id/:type/:name" request
      (get-resource (preprocess request ::resource/params {:id str/upper-case})))
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
