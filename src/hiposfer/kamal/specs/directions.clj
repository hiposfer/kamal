(ns hiposfer.kamal.specs.directions
  (:require [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [hiposfer.geojson.specs :as geojson]
            [hiposfer.kamal.specs.resources :as res]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.services.routing.directions :as dir])
  (:import (java.time LocalDateTime ZoneOffset Duration)))

(s/def ::positive    (s/and spec/number? #(>= % 0)))
(s/def ::name        (s/and string? not-empty))
(s/def ::bearing     (s/and spec/number? #(<= 0 % 360)))

;;TODO this is a copy/paste of the gtfs spec but it gets the job done
(s/def :stop_times/stop ::res/reference)
(s/def :stop_times/trip ::res/reference)
(s/def :stop_times/sequence spec/integer?)
(s/def :stop_times/arrival_time nat-int?)
(s/def :stop_times/departure_time nat-int?)
(s/def ::stop_time (s/keys :req [:stop_times/trip :stop_times/arrival_time
                                 :stop_times/departure_time :stop_times/stop
                                 :stop_times/sequence]))

(s/def :step/name     ::name)
(s/def :step/mode     #{"transit" "walking"})
(s/def :step/distance ::positive)
(s/def :step/duration #(instance? Duration %))
(s/def :step/departure #(instance? LocalDateTime %))
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
(s/def :directions/distance  ::positive)
(s/def :directions/waypoints (s/coll-of ::waypoint :kind sequential? :min-count 2))
(s/def :route/uuid      uuid?)

(s/def ::directions     (s/nilable (s/keys :req [:directions/waypoints
                                                 :directions/distance
                                                 :directions/duration
                                                 :directions/steps])))

;;;;;;;;;;;;;;;;;;;;; REQUEST

;(s/def ::radiuses (s/coll-of (s/or :string #{"unlimited"} :number (s/and int? pos?))))
;(s/def ::language string?)

;; 2017 -> 1483228800
(defn localdatetime-gen []
  (gen/fmap #(LocalDateTime/ofEpochSecond % 0 ZoneOffset/UTC)
            (gen/large-integer* {:min 1483228800 :max 365241780471})))

(s/def ::departure
  (s/with-gen #(instance? LocalDateTime %)
               localdatetime-gen))

(s/def ::area string?)

(s/def ::params (s/keys :req-un [:hiposfer.geojson.specs.linestring/coordinates
                                 ::departure
                                 ::area]))
