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
            [datascript.impl.entity :as dentity]
            [clojure.edn :as edn]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.string :as str])
  (:import (java.time LocalDateTime)))

(def max-distance 1000) ;; meters

(defn- select
  "returns a network whose bounding box contains all points"
  [conns params]
  (when-let [conn (first conns)]
    (if (not (some? (data/entity @conn [:area/id (:area params)])))
      (recur (rest conns) params)
      @conn)))

(defn- match-coordinates
  [network params]
  (for [coord (:coordinates params)
        :let [node (:node/location (first (fastq/nearest-node network coord)))]
        :when (not (nil? node))
        :let [dist (geometry/haversine coord node)]
        :when (< dist max-distance)]
    coord))

(defn- inside?
  "returns a sequence of coordinates matched to the provided ones"
  [network params]
  (let [coords (match-coordinates network params)]
    (when (= (count (:coordinates params)) coords)
      coords)))

(defn- references
  "takes an entity and checks if any of its values are entities, if so replaces
  them by their unique identity value. Then stringifies all keys.

  WARNING: this works only for GTFS entities, since those obey the :name/id
  pattern. Any other reference entity is not guarantee to work"
  [entity]
  (let [data (for [[k v] entity]
               (if (dentity/entity? v)
                 (let [suffix (name k)
                       ident  (keyword suffix "id")]
                     [k {:id (get v ident)}])
                 [k v]))]
    (into {} data)))

(defn- entity
  "try to retrieve an entity from Datascript. Since we dont know if the id is a
  string or a number, we just try both and which one works"
  [network params]
  (let [k (keyword (:name params) "id")]
    (try
      (data/entity network [k (edn/read-string (:id params))])
      (catch Exception _
        (try
          (data/entity network [k (:id params)])
          (catch Exception _ nil))))))

(defn preprocess
  "checks that the passed request conforms to spec and coerce its params
  if so. Returns a possibly modified request.

  We do it like this instead of creating a middleware for readability. We
  need access to the path parameters which are only added after the route
  has been matched.

  See: https://groups.google.com/forum/?hl=en#!topic/compojure/o5l9m7nbGlE"
  [request spec coercer]
  (let [params      (tool/coerce (:params request) coercer)
        errors      (tool/assert params spec)]
    (if (empty? errors)
      (assoc request :params params)
      (assoc request :params params :kamal/errors errors))))

(defn- get-area
  [request]
  (let [regions    (:kamal/networks request)
        ids        (for [conn regions]
                     {:area/id (data/q '[:find ?id . :where [_ :area/id ?id]] @conn)})]
    (code/ok ids)))

(def directions-coercer {:area        str/upper-case
                         :coordinates edn/read-string
                         :departure   #(LocalDateTime/parse %)})

(defn- get-directions
  [request]
  (if (some? (:kamal/errors request))
    (code/bad-request (:kamal/errors request))
    (let [regions (:kamal/networks request)]
      (when-let [network (select regions (:params request))]
        (if (inside? regions (:params request))
          (code/ok (dir/direction network (:params request)))
          (code/ok {:code "NoSegment"
                    :message "No road segment could be matched for coordinates"}))))))

(defn- get-resource
  [request]
  (if (some? (:kamal/errors request))
    (code/bad-request (:kamal/errors request))
    (let [regions (:kamal/networks request)]
      (when-let [network (select regions (:params request))]
        (when-let [e (entity network (:params request))]
          (code/ok (references e)))))))

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  []
  (api/routes
    (api/GET "/area" request (get-area request))
    (api/GET "/area/:area/directions" request
      (get-directions (preprocess request ::dirspecs/params directions-coercer)))
    (api/GET "/area/:area/:name/:id" request
      (get-resource (preprocess request ::resource/params {:area str/upper-case})))
    ;; TODO: implement some persistency for user suggestions
    ;; (api/PUT "/area/:area/suggestions" request
    ;;  (put-suggestions (preprocess request ::dirspecs/params directions-coercer)))
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

;(data/q '[:find ?id .
;          :where [_ :trip/id ?id]]
;         @(first @(:networks (:router hiposfer.kamal.dev/system))))

