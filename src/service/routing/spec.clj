(ns service.routing.spec
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [compojure.api.sweet :refer [context GET resource]]
            [ring.util.http-response :refer [ok]]
            [spec-tools.spec :as spec]
            [service.routing.graph.generators :as g]
            [service.routing.directions :as dir]))

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

; ;(s/def ::vnum3 (s/coll-of number? :kind vector? :count 3 :distinct true :into #{}))
(s/def ::direction (s/keys :req [::code ::waypoints ::routes]
                           :opt []))
(s/def ::code string?)
(s/def ::waypoints (s/cat :name string? :location vector?))
(s/def ::routes (s/coll-of ::route :kind list?))
(s/def ::route (s/keys :req [::geometry ::duration ::distance ::weight ::weight-name ::legs]))
(s/def ::geometry (s/keys :req [::type ::coordinates]))
(s/def ::type string?)
(s/def ::coordinates (s/coll-of ::coordinate :kind list?))
(s/def ::coordinate (s/tuple ::lon ::lon))
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
      :query-params [lon :- ::x, {lat :- ::y 0}]
      :return ::direction
      (ok {:direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}])}))

    (context "/data-plus" []
      (resource
        {:post
         {:summary "data-driven plus with clojure.spec"
          :parameters {:body-params (s/keys :req-un [::x ::y])}
          :responses {200 {:schema ::total-map}}
          :handler (fn [{{:keys [x y]} :body-params}]
                     (ok {:total (+ x y)}))}}))))
