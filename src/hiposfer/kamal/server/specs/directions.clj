(ns hiposfer.kamal.server.specs.directions
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.gtfs.edn :as gtfs]
            [hiposfer.geojson.specs :as geojson]
            [hiposfer.kamal.router.directions :as dir])
  (:import (java.time ZoneOffset ZonedDateTime Instant)))

(s/def ::name    (s/and string? not-empty))
(s/def ::bearing (s/and number? #(<= 0 % 360)))

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

(def trip-keys (set (map :keyword (filter #(= "trips" (:filename %)) gtfs/fields))))
(s/def ::trip (s/map-of trip-keys some?))

(s/def :step/name      ::name)
(s/def :step/mode     #{"transit" "walking"})
(s/def :step/distance #(not (neg? %)))
(s/def :step/duration  (s/and nat-int? #(not (neg? %))))
(s/def :step/departure (s/and nat-int? pos?))
(s/def :step/arrive    (s/and nat-int? pos?))
(s/def :step/maneuver  ::maneuver)
(s/def :step/trip      ::trip)
(s/def :step/geometry  ::geojson/linestring)

(s/def :transit/step (s/keys :req [:step/transit]))
(s/def :base/step    (s/keys :req [:step/mode :step/distance :step/duration
                                   :step/geometry]
                             :opt [:step/name (or :step/arrive :step/departure)]))
(s/def ::step        (s/or :transit (s/merge :base/step :transit/step)
                           :walk    (s/and :base/step #(not (contains? % :step/transit)))))

(s/def :waypoint/name     (s/nilable ::name))
(s/def :waypoint/location :hiposfer.geojson.specs.point/coordinates)
(s/def ::waypoint         (s/keys :req [:waypoint/name :waypoint/location]))

(s/def :directions/steps      (s/coll-of ::step :kind sequential?))
(s/def :directions/distance  #(not (neg? %)))
(s/def :directions/waypoints  (s/coll-of ::waypoint :kind sequential? :min-count 2))
(s/def :route/uuid            uuid?)

(s/def ::directions (s/nilable (s/keys :req [:directions/waypoints
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
