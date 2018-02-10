(ns hiposfer.kamal.specs.mapbox.directions
  (:require [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [hiposfer.geojson.specs :as geojson]))

;;;;;;;;;;;;;;;;;;;;;;;;;; RESPONSE

(defn natural? "positive or zero number" [number] (or (pos? number) (zero? number)))

;; todo ::annotation => https://www.mapbox.com/api-documentation/#routeleg-object
(s/def ::code        string?);;it actually should be an enum i.e. one of "bla bla" or "foo"
(s/def ::name        string?)
(s/def ::summary     string?)
(s/def ::type        string?);;it actually should be an enum i.e. one of "bla bla" or "foo"
(s/def ::mode        string?);;it actually should be an enum i.e. one of "bla bla" or "foo"
(s/def ::instruction string?)
(s/def ::modifier    string?)
(s/def ::bearing     (s/and spec/number? #(<= 0 % 360)))
(s/def ::bearing_before ::bearing)
(s/def ::bearing_after  ::bearing)
(s/def ::location    ::geojson/position)
(s/def ::duration    (s/and spec/number? natural?))
(s/def ::distance    (s/and spec/number? natural?))
(s/def ::weight      (s/and spec/number? natural?)) ;; a negative weight doesnt make sense
(s/def ::weight_name string?)
;; structures
(s/def ::maneuver    (s/keys :req-un [::location      ::bearing_before
                                      ::bearing_after ::instruction
                                      ::type]
                             :opt-un [::modifier]))
(s/def ::route-step  (s/keys :req-un [::distance ::duration ::geometry
                                      ::name     ::mode     ::maneuver]))
(s/def ::steps       (s/coll-of ::route-step :kind sequential?))
(s/def ::route-leg   (s/keys :req-un [::distance ::duration ::steps ::summary]))
(s/def ::legs        (s/coll-of ::route-leg :kind sequential?))
(s/def ::waypoint    (s/keys :req-un [::name ::location]))
(s/def ::waypoints   (s/coll-of ::waypoint :kind sequential? :min-count 2))
(s/def ::geometry    ::geojson/linestring)
(s/def ::route       (s/keys :req-un [::geometry ::duration    ::distance
                                      ::weight   ::weight_name ::legs]))
(s/def ::routes      (s/coll-of ::route :kind sequential?))
(s/def ::response   (s/keys :req-un [::code] :opt-un [::waypoints ::routes]))

;;;;;;;;;;;;;;;;;;;;; REQUEST

(s/def ::radiuses (s/coll-of #(or (= "unlimited" %) (pos? %))))
(s/def ::language string?)
;; TODO: dirty hack to avoid separating namespaces
(s/def :mapbox.directions.request/steps boolean?)

(s/def ::args (s/keys :req-un [:hiposfer.geojson.specs.multipoint/coordinates]
                      :opt-un [::radiuses
                               :mapbox.directions.request/steps]))

;; TODO: pull syntax for filtering data before sending response
