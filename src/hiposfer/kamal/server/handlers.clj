(ns hiposfer.kamal.server.handlers
  (:require [datascript.core :as data]
            [clojure.edn :as edn]
            [compojure.core :as api]
            [compojure.route :as route]
            [ring.util.io :as rio]
            [ring.util.response :as response]
            [ring.util.http-response :as code]
            [hiposfer.kamal.router.directions :as dir]
            [hiposfer.kamal.server.specs.directions :as dirspecs]
            [hiposfer.kamal.server.specs.resources :as resource]
            [hiposfer.kamal.router.util.geometry :as geometry]
            [hiposfer.kamal.router.util.fastq :as fastq]
            [hiposfer.kamal.router.util.misc :as misc]
            [hiposfer.kamal.router.io.gtfs :as gtfs]
            [clojure.java.io :as io])
  (:import (java.time ZonedDateTime)
           (java.util Properties)))

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
        network (deref (:area/conn params))
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
                             (reduced conn)))
                         nil
                         networks)]
    (if (some? network)
      (assoc-in request [:params :area/conn] network)
      (code/bad-request! {:msg "unknown area" :data (:area params)}))))

(defn validate
  "Throws an exception if request doesnt conform to spec, returns the
   request otherwise.

  Accepts an optional keyword k to specify which part of the request to
  validate. Defaults to :params

  We do it like this instead of creating a middleware for readability. We
  need access to the path parameters which are only added after the route
  has been matched.

  See: https://groups.google.com/forum/?hl=en#!topic/compojure/o5l9m7nbGlE"
  ([request spec]
   (validate request :params spec))
  ([request k spec]
   (let [errors (misc/assert (get request k) spec)]
     (if (not (some? errors)) request
       (code/bad-request! errors)))))

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
  (let [conn     (:area/conn (:params request))
        response (dir/direction conn (:params request))]
    (if (some? response)
      (code/ok response)
      (code/precondition-failed
        {:code "NoRoute"
         :msg (str "There was no route found for the given coordinates. Check for "
                   "impossible routes (e.g. routes over oceans without ferry connections).")}))))

(defn- get-resource
  [request]
  (let [network (deref (:area/conn (:params request)))
        k       (keyword (:name (:params request)) "id")
        v       (gtfs/decode (:id (:params request)))]
    (code/ok (gtfs/resource (data/entity network [k v])))))

(defn- query-area
  [request]
  (let [network (data/filter @(:area/conn (:params request))
                              (fn [_ datom] (contains? gtfs/keywords (:a datom))))
        q       (:q (:params request))
        args    (:args (:params request))]
    (code/ok (apply data/q q (cons network args)))))

(defn- update-area
  [request]
  (let [conn (:area/conn (:params request))
        db   (:db-after (data/with @conn (:body request)))]
    (-> (rio/piped-input-stream #(gtfs/dump! db %))
        (code/ok)
        (response/content-type "application/zip")
        (assoc-in [:headers "Content-Disposition"]
                  "attachment; filename=\"gtfs.zip\""))))

(defn- read-pom
  "reads the pom properties file of Ahead of Time compiled code and returns the
  version of the project"
  [group-id artifact]
  (let [pom        (str "META-INF/maven/" group-id "/" artifact "/pom.properties")
        properties (doto (new Properties)
                     (.load (io/reader (io/resource pom))))]
    (into {} properties)))

;; ring handlers are matched in order
(def server "all API handlers"
  (api/routes
    (api/GET "/" request
      (code/ok {:version (get (read-pom "hiposfer" "kamal") "version")}))
    (api/GET "/area" request (get-area request))
    (api/GET "/area/:area/directions" request
      (-> (update request :params misc/coerce directions-coercer)
          (validate ::dirspecs/params)
          (select-network)
          (validate-coordinates)
          (get-directions)))
    (api/GET "/area/:area/:name/:id" request
      (-> (validate request ::resource/params)
          (select-network)
          (get-resource)))
    (api/GET "/area/:area/gtfs" request
      (-> (update request :params misc/coerce {:q    edn/read-string
                                               :args edn/read-string})
          (validate ::resource/query)
          (select-network)
          (query-area)))
    (api/POST "/area/:area/gtfs" request
      (-> (select-network request)
          (validate :body ::resource/transaction)
          (update-area)))
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

#_(data/q '[:find (pull ?agency [*])
            :where [?agency :agency/id ?id]]
           @(first @(:networks (:router hiposfer.kamal.dev/system))))
