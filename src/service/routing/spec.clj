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

; (s/def :arc/kind (set (keys osm/speeds)))
; (s/def :arc/length (s/and int? pos?))
; (s/def :arc/dst int?)
; (s/def :arc/src int?)

(s/def ::lon (s/and number? #(<= -180 % 180)))
(s/def ::lat (s/and number? #(<= -90 % 90)))
; (s/def :node/arcs (s/coll-of :graph/arc :kind list?))
; (s/def :node/out-arcs :node/arcs)
; (s/def :node/in-arcs :node/arcs)

; (s/def :graph/arc (s/and (s/keys :req-un [:arc/src :arc/dst :arc/length :arc/kind])
;                          (fn [arc] (not= (:src arc) (:dst arc)))))
; (s/def :graph/node (s/and (s/keys :req-un [:node/lat :node/lon :node/out-arcs :node/in-arcs])
;                           (fn [node] (or (not-empty (:out-arcs node)) (not-empty (:in-arcs node))))))
; (s/def :int-map/graph (s/map-of int? :graph/node))

(s/def ::x spec/int?)
(s/def ::y spec/int?)
(s/def ::total spec/int?)
(s/def ::total-map (s/keys :req-un [::total]))

(s/def ::direction (s/nilable (s/keys :req-un [::code ::waypoints ::routes])))
(s/def ::code string?)
(s/def ::name string?)
(s/def ::location (s/coll-of number? :kind vector? :count 2 :into #{}))
(s/def ::waypoints (s/coll-of ::waypoint-object :kind sequential? :min-count 2))
(s/def ::waypoint-object (s/keys :req-un [::name ::location]))
(s/def ::routes (s/coll-of ::route-object :kind sequential?))
(s/def ::route-object (s/keys :req-un [::geometry ::duration ::distance ::weight ::weight-name ::legs]))
(s/def ::geometry (s/keys :req-un [::type ::coordinates]))
(s/def ::type string?)
(s/def ::coordinates (s/coll-of ::coordinate-object :kind sequential?))
(s/def ::coordinate-object (s/tuple ::lon ::lat))
(s/def ::duration number?)
(s/def ::distance number?)
(s/def ::weight number?)
(s/def ::weight-name number?)
(s/def ::legs vector?)

(def routes
  (context "/spec" []
    :tags ["spec"]
    :coercion :spec

    (GET "/direction" []
      :summary "direction with clojure.spec"
      :query-params [lon :- ::x, lat :- ::y]
      :return ::direction
      (ok (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}])))))

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
