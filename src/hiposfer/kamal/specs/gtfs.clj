(ns hiposfer.kamal.specs.gtfs
  (:require [clojure.spec.alpha :as s]
            [hiposfer.geojson.specs :as geojson]))

;; TODO: here are only the obligatory fields

;; agencies
(s/def ::agency_id some?)
(s/def ::agency_url string?)
(s/def ::agency_name string?)
(s/def ::agency_timezone string?)
(s/def ::agency (s/keys :req-un [::agency_name ::agency_timezone]
                        :opt-un [::agency_id]))
;; routes
(s/def ::route_id some?)
(s/def ::route_short_name string?)
(s/def ::route_long_name string?)
(s/def ::route_type int?)
(s/def ::route (s/keys :req-un [::route_id ::route_short_name ::route_type]
                       :opt-un [::agency_id]))

;; stops
(s/def ::stop_lon ::geojson/lon)
(s/def ::stop_lat ::geojson/lat)
(s/def ::stop_id  some?)
(s/def ::stop_name string?)
(s/def ::stop (s/keys :req-un [::stop_lon ::stop_lat ::stop_id ::stop_name]))

;; trips
(s/def ::service_id some?)
(s/def ::trip_id some?)
(s/def ::trip (s/keys :req-un [::route_id ::service_id ::trip_id]))

;; stop_times
;; HH:MM:SS
(defn time? [text] (re-matches #"\d{2}:\d{2}:\d{2}" text))

(s/def ::trip_id some?)
(s/def ::arrival_time (s/and string? time?))
(s/def ::departure_time (s/and string? time?))
(s/def ::stop_sequence (s/and (s/coll-of (s/and int? pos?))
                              #(apply < %)))
(s/def ::stop (s/keys :req-un [::trip_id ::arrival_time ::departure_time
                               ::stop_id ::stop_sequence]))

;; calendar
;; AAAAMMDD
(defn date? [text] (re-matches #"\d{8}" text))

(s/def ::monday boolean?)
(s/def ::tuesday boolean?)
(s/def ::wednesday boolean?)
(s/def ::thursday boolean?)
(s/def ::friday boolean?)
(s/def ::saturday boolean?)
(s/def ::sunday boolean?)
(s/def ::start_date (s/and string? date?))
(s/def ::stop_date (s/and string? date?))
(s/def ::calendar (s/keys :req-un [::service_id ::monday ::tuesday
                                   ::wednesday ::thursday ::friday
                                   ::saturday ::sunday ::start_date
                                   ::stop_date]))
