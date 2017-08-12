(ns service.routing.spec
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]))
            ;[clojure.spec.test.alpha :as stest]))
            ;[expound.alpha :as expound]))

(defn- parse-coordinates
  [text]
  (let [pairs (str/split text #";")]
    (for [coords pairs]
      (mapv edn/read-string (str/split coords #",")))))
;(->coordinates "-122.42,37.78;-77.03,38.91")

(defn- parse-radiuses
  [text]
  (map edn/read-string (str/split text #";")))
;(->radiuses "1200.50;100;500;unlimited;100")

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

(s/def ::coordinates-raw
  (s/and string? #(re-matches coordinate-regex %)
         (s/conformer parse-coordinates)
         ::coordinates))

(s/def ::radius (s/or :unlimited #(= "unlimited" %) :distance (s/and number? #(> % 0))))
(s/def ::radiuses (s/coll-of ::radius))
(s/def ::radiuses-raw
  (s/and string? #(re-matches rads-regex %)
         (s/conformer parse-radiuses)
         ::radiuses))