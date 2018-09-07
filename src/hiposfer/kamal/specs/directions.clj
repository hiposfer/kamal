(ns hiposfer.kamal.specs.directions
  (:require [clojure.spec.alpha :as s]
            [hiposfer.geojson.specs :as geojson]
            [hiposfer.kamal.specs.resources :as res]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.services.routing.directions :as dir]
            [hiposfer.kamal.parsers.gtfs.core :as gtfs]
            [clojure.string :as str])
  (:import (java.time ZoneOffset ZonedDateTime Instant)))

(s/def ::name        (s/and string? not-empty))
(s/def ::bearing     (s/and number? #(<= 0 % 360)))

(def stop-time-keys (set (filter #(str/starts-with? (name %) "stop_time")
                                  (gtfs/attributes))))
(s/def ::stop_time (s/map-of stop-time-keys any?))

(s/def :step/name     ::name)
(s/def :step/mode     #{"transit" "walking"})
(s/def :step/distance #(not (neg? %)))
(s/def :step/duration (s/and nat-int? #(not (neg? %))))
(s/def :step/departure (s/and nat-int? pos?))
(s/def :step/geometry   ::geojson/linestring)

(s/def :maneuver/instruction    (s/and string? not-empty))
(s/def :maneuver/modifier       (set (vals dir/bearing-turns)))
(s/def :maneuver/type           #{"turn" "depart" "arrive" "exit vehicle"
                                  "notification" "continue"})
(s/def :maneuver/bearing_before ::bearing)
(s/def :maneuver/bearing_after  ::bearing)

(s/def ::maneuver  (s/keys :req [:maneuver/bearing_before
                                 :maneuver/bearing_after
                                 :maneuver/instruction]
                           :opt [:maneuver/modifier]))

(s/def :base/step  (s/merge (s/keys :req [:step/mode :step/distance
                                          :step/duration :step/geometry]
                                    :opt [:step/name])
                            ::maneuver))

(s/def ::step      (s/or :transit (s/merge :base/step ::stop_time)
                         :walk    :base/step))

(s/def :waypoint/name     (s/nilable ::name))
(s/def :waypoint/location :hiposfer.geojson.specs.point/coordinates)
(s/def ::waypoint         (s/keys :req [:waypoint/name :waypoint/location]))

(s/def :directions/steps     (s/coll-of ::step :kind sequential?))
(s/def :directions/distance  #(not (neg? %)))
(s/def :directions/waypoints (s/coll-of ::waypoint :kind sequential? :min-count 2))
(s/def :route/uuid           uuid?)

(s/def ::directions     (s/nilable (s/keys :req [:directions/waypoints
                                                 :directions/distance
                                                 :directions/duration
                                                 :directions/steps])))

;;;;;;;;;;;;;;;;;;;;; REQUEST

;(s/def ::radiuses (s/coll-of (s/or :string #{"unlimited"} :number (s/and int? pos?))))
;(s/def ::language string?)

;; 2017 -> 1483228800
(defn datetime-generator []
  (gen/fmap #(ZonedDateTime/ofInstant (Instant/ofEpochSecond %) ZoneOffset/UTC)
             (gen/large-integer* {:min 1483228800 :max 365241780471})))

(s/def ::departure
  (s/with-gen #(instance? ZonedDateTime %)
               datetime-generator))

(s/def ::area string?)

(s/def ::params (s/keys :req-un [:hiposfer.geojson.specs.linestring/coordinates
                                 ::departure
                                 ::area]))
