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
            [hiposfer.kamal.parsers.gtfs :as gtfs])
  (:import (java.time ZonedDateTime)))

(def max-distance 1000) ;; meters

(defn- select
  "returns a network whose bounding box contains all points"
  [conns params]
  (if-let [conn (first conns)]
    (if (some? (data/entity @conn [:area/id (:area params)]))
      (deref conn)
      (recur (rest conns) params))
    (code/bad-request! {:msg "unknown area" :data (:area params)})))

(defn- match-coordinates
  [network params]
  (for [coord (:coordinates params)
        :let [node (:node/location (first (fastq/nearest-nodes network coord)))]
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
    (if (some? errors)
      (code/bad-request! errors)
      (assoc request :params params))))

(defn- get-area
  [request]
  (let [regions (:kamal/networks request)
        areas   (for [conn regions]
                  (let [id (data/q '[:find ?area .
                                     :where [?area :area/name]]
                                   @conn)]
                    (into {} (data/entity @conn id))))]
    (code/ok areas)))

(def directions-coercer {:coordinates edn/read-string
                         :departure   #(ZonedDateTime/parse %)})

(defn- get-directions
  [request]
  (let [networks (:kamal/networks request)
        network  (select networks (:params request))]
    (if (inside? network (:params request))
      (let [response (dir/direction network (:params request))]
        (if (some? response)
          (code/ok response)
          (code/precondition-failed
            {:code "NoRoute"
             :msg "There was no route found for the given coordinates. Check for
                   impossible routes (e.g. routes over oceans without ferry connections)."})))
      (code/precondition-failed
        {:code "NoSegment"
         :msg  "No road segment could be matched for coordinates"}))))

(defn- get-resource
  [request]
  (let [regions (:kamal/networks request)
        network (select regions (:params request))
        k       (keyword (:name (:params request)) "id")
        v       (gtfs/coerce (:id (:params request)))]
    (code/ok (gtfs/resource (data/entity network [k v])))))

(defn- query-area
  [request]
  (let [regions (:kamal/networks request)
        network (select regions (:params request))
        q       (:q (:params request))
        args    (:args (:params request))]
    (code/ok (apply data/q q (cons network args)))))

;; ring handlers are matched in order
(defn create
  "creates an API handler with a closure around the router"
  []
  (api/routes
    (api/GET "/area" request (get-area request))
    (api/GET "/area/:area/directions" request
      (-> (preprocess request ::dirspecs/params directions-coercer)
          (get-directions)))
    (api/GET "/area/:area/:name/:id" request
      (-> (preprocess request ::resource/params {})
          (get-resource)))
    (api/GET "/area/:area" request
      (-> (preprocess request ::resource/query {:q edn/read-string
                                                :args edn/read-string})
          (query-area)))
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
