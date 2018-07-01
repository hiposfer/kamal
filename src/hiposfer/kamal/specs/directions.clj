(ns hiposfer.kamal.specs.directions
  (:require [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [hiposfer.geojson.specs :as geojson]
            [clojure.spec.gen.alpha :as gen]
            [hiposfer.kamal.services.routing.directions :as dir]
            [clojure.edn :as edn])
  (:import (java.time LocalDateTime ZoneOffset)))

;; TODO: these functions look ugly as hell. I dont think we are modelling it
;; the right way but it gets the job done for the time being.
(defn- consistent-maneuver?
  [step]
  (case (:mode step)
    "transit" (contains? #{"notification" "continue" "exit vehicle"}
                         (:type (:maneuver step)))
    "walking" (contains? #{"depart" "arrive" "turn"}
                         (:type (:maneuver step)))))

(defn- vehicle-info?
  [man]
  (if (not= "notification" (:type man)) true
    (and (s/valid? ::wait (:wait man))
         (s/valid? ::trip (:trip man)))))

(defn- natural? [n] (>= n 0))

;;;;;;;;;;;;;;;;;;;;;;;;;; RESPONSE

;; todo ::annotation => https://www.mapbox.com/api-documentation/#routeleg-object
(s/def ::code        #{"Ok" "NoRoute" "NoSegment"})
(s/def ::name        string?)
(s/def ::summary     string?)
(s/def ::type        #{"turn" "exit vehicle" "notification" "continue" "depart" "arrive"})
(s/def ::mode        #{"walking" "transit"})
(s/def ::instruction (s/and string? not-empty))
(s/def ::modifier    (set (vals dir/bearing-turns)))
(s/def ::positive    (s/and spec/number? natural?))
(s/def ::wait        ::positive)
(s/def ::trip        double?)
(s/def ::bearing     (s/and spec/number? #(<= 0 % 360)))
(s/def ::bearing_before ::bearing)
(s/def ::bearing_after  ::bearing)
(s/def ::location    ::geojson/position)
(s/def ::duration    ::positive)
(s/def ::distance    ::positive)
(s/def ::weight      ::positive)
(s/def ::weight_name string?)
;; structures
(s/def ::maneuver    (s/and (s/keys :req-un [::location ::bearing_before ::bearing_after
                                             ::instruction ::type]
                                    :opt-un [::modifier])
                            vehicle-info?))
(s/def ::route-step  (s/and (s/keys :req-un [::distance ::duration ::geometry
                                             ::name     ::mode     ::maneuver])
                            consistent-maneuver?))
(s/def ::steps       (s/coll-of ::route-step :kind sequential?))
(s/def ::route-leg   (s/keys :req-un [::distance ::duration ::steps ::summary]))
(s/def ::legs        (s/coll-of ::route-leg :kind sequential?))
(s/def ::waypoint    (s/keys :req-un [::name ::location]))
(s/def ::waypoints   (s/coll-of ::waypoint :kind sequential? :min-count 2))
(s/def ::geometry    ::geojson/linestring)
(s/def ::route       (s/keys :req-un [::duration ::distance
                                      ::weight   ::weight_name ::legs]))
(s/def ::routes      (s/coll-of ::route :kind sequential?))
(s/def ::response   (s/keys :req-un [::code] :opt-un [::waypoints ::routes]))

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
