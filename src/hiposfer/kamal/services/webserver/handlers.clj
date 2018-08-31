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
  (:import (java.time ZonedDateTime)))

(def max-distance 1000) ;; meters

(defn- select
  "returns a network whose bounding box contains all points"
  [conns params]
  (when-let [conn (first conns)]
    (if (not (some? (data/entity @conn [:area/name (:area params)])))
      (recur (rest conns) params)
      @conn)))

(defn- match-coordinates
  [network params]
  (for [coord (:coordinates params)
        :let [node (:node/location (first (fastq/nearest-node network coord)))]
        :when (some? node)
        :let [dist (geometry/haversine coord node)]
        :when (< dist max-distance)]
    coord))

(defn- inside?
  "returns a sequence of coordinates matched to the provided ones"
  [network params]
  (let [coords (match-coordinates network params)]
    (when (= (count (:coordinates params)) (count coords))
      coords)))

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
  (let [regions (:kamal/networks request)
        areas   (for [conn regions]
                  (let [id (data/q '[:find ?area .
                                     :where [?area :area/name]]
                                   @conn)]
                    (into {} (data/entity @conn id))))]
    (code/ok areas)))

(def directions-coercer {:area        #(str/replace % "_" " ")
                         :coordinates edn/read-string
                         :departure   #(ZonedDateTime/parse %)})

(defn- get-directions
  [request]
  (if (some? (:kamal/errors request))
    (code/bad-request (:kamal/errors request))
    (let [networks (:kamal/networks request)]
      (when-let [network (select networks (:params request))]
        (if (not (inside? network (:params request)))
          (code/precondition-failed
            {:code    "NoSegment"
             :message "No road segment could be matched for coordinates"})
          (let [response (dir/direction network (:params request))]
            (if (not (some? response))
              (code/precondition-failed
                {:code "NoRoute"
                 :message "There was no route found for the given coordinates. Check for
                       impossible routes (e.g. routes over oceans without ferry connections)."})
              (code/ok response))))))))

(defn- get-resource
  [request]
  (if (some? (:kamal/errors request))
    (code/bad-request (:kamal/errors request))
    (let [regions (:kamal/networks request)]
      (when-let [network (select regions (:params request))]
        (when-let [e (entity network (:params request))]
          (code/ok (tool/gtfs-resource e)))))))

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
;  (direction @(first @(:networks (:router hiposfer.kamal.dev/system)))
;             {:coordinates [[8.645333, 50.087314]
;                            [8.635897, 50.104172]]
;              :departure (ZonedDateTime/parse "2018-05-07T10:15:30+02:00")
;              :steps true}))

;(data/q '[:find ?id .
;          :where [_ :trip/id ?id]]
;         @(first @(:networks (:router hiposfer.kamal.dev/system))))
