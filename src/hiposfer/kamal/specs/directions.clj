(ns hiposfer.kamal.specs.directions
  (:require [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [hiposfer.geojson.specs :as geojson]
            [hiposfer.kamal.specs.resources :as res]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.services.routing.directions :as dir])
  (:import (java.time LocalDateTime ZoneOffset)))

;;TODO this is a copy/paste of the gtfs spec but it gets the job done
(s/def :stop_times/stop ::res/resource)
(s/def :stop_times/trip ::res/resource)
(s/def :stop_times/sequence spec/integer?)
(s/def :stop_times/arrival_time nat-int?)
(s/def :stop_times/departure_time nat-int?)
(s/def ::stop_time (s/keys :req [:stop_times/trip :stop_times/arrival_time
                                 :stop_times/departure_time :stop_times/stop
                                 :stop_times/sequence]))

(s/def ::code        #{"Ok" "NoRoute" "NoSegment"})
(s/def ::name        string?)
;;(s/def ::summary     string?)
(s/def :walk/type    #{"turn" "depart" "arrive"})
(s/def :transit/type #{"exit vehicle" "notification" "continue"})
(s/def :walk/mode    #{"walking"})
(s/def :transit/mode #{"transit"})
(s/def ::instruction (s/and string? not-empty))
(s/def ::modifier    (set (vals dir/bearing-turns)))
(s/def ::positive    (s/and spec/number? #(>= % 0)))
(s/def ::bearing     (s/and spec/number? #(<= 0 % 360)))
(s/def ::bearing_before ::bearing)
(s/def ::bearing_after  ::bearing)
(s/def ::duration    ::positive)
(s/def ::distance    ::positive)
(s/def ::weight      ::positive)
(s/def ::weight_name string?)
;; structures
(s/def :maneuver/base    (s/keys :req-un [::bearing_before ::bearing_after
                                          ::instruction]
                                 :opt-un [::modifier]))

(s/def :walk/maneuver    (s/merge :maneuver/base (s/keys :req-un [:walk/type])))
(s/def :transit/maneuver (s/merge :maneuver/base (s/keys :req-un [:transit/type])))

(s/def :step/base  (s/keys :req-un [::distance ::duration ::geometry]
                           :opt-un [::name]))

(s/def :walk/step  (s/merge (s/keys :req-un [:walk/mode :walk/maneuver])
                            :step/base))

(s/def :transit/step (s/merge (s/keys :req-un [:transit/mode :transit/maneuver])
                              :step/base
                              ::stop_time))

(s/def ::step        (s/or :transit :transit/step
                           :walk :walk/step))
(s/def ::steps       (s/coll-of ::step :kind sequential?))
;;(s/def ::leg         (s/keys :req-un [::distance ::duration ::steps])) ;;::summary]))
;;(s/def ::legs        (s/coll-of ::route-leg :kind sequential?))
(s/def ::waypoint    (s/keys :req-un [::name ::location]))
(s/def ::waypoints   (s/coll-of ::waypoint :kind sequential? :min-count 2))
(s/def ::geometry    ::geojson/linestring)
(s/def ::route       (s/keys :req-un [::distance ::duration ::steps]))
;;(s/def ::routes      (s/coll-of ::route :kind sequential?))
(s/def ::response   (s/keys :req-un [::code] :opt-un [::waypoints ::route]))

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
