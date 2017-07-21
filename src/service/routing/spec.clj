(ns service.routing.spec
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [compojure.api.sweet :refer [context GET resource]]
            [ring.util.http-response :refer [ok]]
            [spec-tools.spec :as spec]
            [service.routing.graph.generators :as g]
            [service.routing.directions :as dir]
            [expound.alpha :as expound]))
            ;[clojure.spec.test.alpha :as stest]))

(s/def ::lon (s/and spec/number? #(<= -180 % 180)))
(s/def ::lat (s/and spec/number? #(<= -90 % 90)))
(s/def ::direction (s/keys :req-un [::code] :opt-un [::waypoints ::routes]))
(s/def ::code string?)
(s/def ::name string?)
(s/def ::location (s/tuple ::lon ::lat))
(s/def ::waypoints (s/coll-of ::waypoint-object :kind sequential? :min-count 2))
(s/def ::waypoint-object (s/keys :req-un [::name ::location]))
(s/def ::routes (s/coll-of ::route-object :kind sequential?))
(s/def ::route-object (s/keys :req-un [::geometry ::duration ::distance ::weight ::weight-name ::legs]))
(s/def ::geometry (s/keys :req-un [::type ::coordinates]))
(s/def ::type string?)
(s/def ::coordinates (s/coll-of ::coordinate-object :kind sequential?))
(s/def ::coordinate-object (s/tuple ::lon ::lat))
(s/def ::duration spec/number?)
(s/def ::distance spec/number?)
(s/def ::weight spec/number?)
(s/def ::weight-name string?)
(s/def ::legs vector?)

(s/def ::point (s/tuple double? double?))

(def routes
  (context "/spec" []
    :tags ["spec"]
    :coercion :spec

    (GET "/direction" []
      :summary "direction with clojure.spec"
      :query-params [start_lon :- ::lon, start_lat :- ::lat, dest_lon :- ::lon, dest_lat :- ::lat]
      :return ::direction
      (ok (dir/direction (gen/generate (g/graph 1000))
                          :coordinates [{:lon start_lon :lat start_lat} {:lon dest_lon :lat dest_lat}])))))

    ; (context "/data-plus" []
    ;   (resource
    ;     {:post
    ;      {:summary "data-driven plus with clojure.spec"
    ;       :parameters {:body-params (s/keys :req-un [::x ::y])}
    ;       :responses {200 {:schema ::total-map}}
    ;       :handler (fn [{{:keys [x y]} :body-params}]
    ;                  (ok {:total (+ x y)}))}}))


;;;; TEST @mehdi
;(expound/expound-str ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))
;(s/explain ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))
