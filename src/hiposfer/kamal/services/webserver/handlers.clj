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

(defn- match-coordinates
  [network params]
  (for [coord (:coordinates params)
        :let [node (:node/location (first (fastq/nearest-nodes network coord)))]
        :when (some? node)
        :let [dist (geometry/haversine coord node)]
        :when (< dist max-distance)]
    coord))

(defn- validate-coordinates
  "returns a sequence of coordinates matched to the provided ones"
  [request]
  (let [params  (:params request)
        network (:network params)
        coords (match-coordinates network params)]
    (if (= (count (:coordinates params)) (count coords)) request
      (code/precondition-failed!
        {:code "NoSegment"
         :msg  "No road segment could be matched for coordinates"}))))

(defn- select-network
  "returns a network whose bounding box contains all points"
  [request]
  (let [networks (:kamal/networks request)
        params   (:params request)
        network  (reduce (fn [_ conn]
                           (when (some? (data/entity @conn [:area/id (:area params)]))
                             (reduced (deref conn))))
                         nil
                         networks)]
    (if (some? network)
      (assoc-in request [:params :network] network)
      (code/bad-request! {:msg "unknown area" :data (:area params)}))))

(defn validate-params
  "checks that the passed request conforms to spec and coerce its params
  if so. Returns a possibly modified request.

  We do it like this instead of creating a middleware for readability. We
  need access to the path parameters which are only added after the route
  has been matched.

  See: https://groups.google.com/forum/?hl=en#!topic/compojure/o5l9m7nbGlE"
  [request spec]
  (let [errors      (tool/assert (:params request) spec)]
    (if (not (some? errors)) request
      (code/bad-request! errors))))

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
  (let [network  (:network (:params request))
        response (dir/direction network (:params request))]
    (if (some? response)
      (code/ok response)
      (code/precondition-failed
        {:code "NoRoute"
         :msg (str "There was no route found for the given coordinates. Check for "
                   "impossible routes (e.g. routes over oceans without ferry connections).")}))))

(defn- get-resource
  [request]
  (let [network (:network (:params request))
        k       (keyword (:name (:params request)) "id")
        v       (gtfs/coerce (:id (:params request)))]
    (code/ok (gtfs/resource (data/entity network [k v])))))

(defn- query-area
  [request]
  (let [network (data/filter (:network (:params request))
                             (fn [_ datom] (contains? gtfs/attributes (:a datom))))
        q       (:q (:params request))
        args    (:args (:params request))]
    (code/ok (apply data/q q (cons network args)))))

;; ring handlers are matched in order
(def server "all API handlers"
  (api/routes
    (api/GET "/" request
      (code/ok {:version (System/getProperty "kamal.version")}))
    (api/GET "/area" request (get-area request))
    (api/GET "/area/:area/directions" request
      (-> (update request :params tool/coerce directions-coercer)
          (validate-params ::dirspecs/params)
          (select-network)
          (validate-coordinates)
          (get-directions)))
    (api/GET "/area/:area/:name/:id" request
      (-> (validate-params request ::resource/params)
          (select-network)
          (get-resource)))
    (api/GET "/area/:area/gtfs" request
      (-> (update request :params tool/coerce {:q    edn/read-string
                                               :args edn/read-string})
          (validate-params ::resource/query)
          (select-network)
          (query-area)))
    (api/PUT "/area/:area/gtfs" request
      (-> #_(update request :params tool/coerce {:q    edn/read-string
                                                 :args edn/read-string})
          #_(validate-params ::resource/query)
          #_(select-network)
          #_(query-area)
          (code/ok (:body request))))
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
