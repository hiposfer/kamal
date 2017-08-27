(ns service.routing.spec
  (:require [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]))
            ;[expound.alpha :as expound]))

(def coordinate-regex #"(-?\d+(\.\d+)?),(-?\d+(\.\d+)?)(;(-?\d+(\.\d+)?),(-?\d+(\.\d+)?))+")
(def rads-regex #"((\d+(\.\d+)?)|unlimited)(;((\d+(\.\d+)?)|unlimited))*")


(s/def ::lon (s/and spec/number? #(<= -180 % 180)))
(s/def ::lat (s/and spec/number? #(<= -90 % 90)))
(s/def ::code string?)
(s/def ::name string?)
(s/def ::coordinate-object (s/tuple ::lon ::lat))
(s/def ::location ::coordinate-object)
(s/def ::type string?)
(s/def ::coordinates (s/coll-of ::coordinate-object :kind sequential?))
(s/def ::duration (s/and spec/number? pos?))
(s/def ::distance (s/and spec/number? pos?))
(s/def ::weight spec/number?)
(s/def ::weight_name string?)
(s/def ::legs vector?)
(s/def ::waypoint-object (s/keys :req-un [::name ::location]))
(s/def ::waypoints (s/coll-of ::waypoint-object :kind sequential? :min-count 2))
(s/def ::geometry (s/keys :req-un [::type ::coordinates]))
(s/def ::route-object (s/keys :req-un [::geometry ::duration ::distance ::weight ::weight_name ::legs]))
(s/def ::routes (s/coll-of ::route-object :kind sequential?))
(s/def ::direction (s/keys :req-un [::code] :opt-un [::waypoints ::routes]))

(s/def ::raw-coordinates (s/and string? #(re-matches coordinate-regex %)))

(s/def ::radiuses (s/coll-of #(or (= "unlimited" %) (pos? %))))
(s/def ::raw-radiuses (s/and string? #(re-matches rads-regex %)))