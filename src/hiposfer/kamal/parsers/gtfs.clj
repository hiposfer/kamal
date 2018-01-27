(ns hiposfer.kamal.parsers.gtfs
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]
            [hiposfer.geojson.specs :as geojson]
            [clojure.string :as str]))

(s/def ::agency_id spec/integer?)
(s/def ::agency_name string?)
(s/def ::agency_timezone string?)
(s/def ::agency (s/keys :req-un [::agency_id ::agency_name]
                        :opt-un [::agency_timezone]))

(s/def ::route_id (s/and spec/integer?))
(s/def ::route_short_name string?)
(s/def ::route_long_name string?)
(s/def ::route_type spec/integer?)
(s/def ::route_url string?)
(s/def ::route (s/keys :req-un [::route_id ::agency_id ::route_short_name
                                ::route_type ::route_url]))

(s/def ::stop_lon (s/and spec/number? ::geojson/lon))
(s/def ::stop_lat (s/and spec/number? ::geojson/lat))
(s/def ::stop_id  spec/integer?)
(s/def ::stop_name string?)
(s/def ::stop (s/keys :req-un [::stop_lon ::stop_lat ::stop_id ::stop_name]))

(def types ;; not all filename correspond to a type so we map them here
  {"agency.txt" ::agency
   "routes.txt" ::route
   "stops.txt" ::stop})

(defn parse
  [filename]
  (with-open [file (io/reader filename)]
    (let [type    (types (last (str/split filename #"/")))
          raw     (csv/read-csv file)
          head    (map keyword (first raw))
          content (map zipmap (repeat head) (rest raw))]
      (into [] (remove #(= % ::s/invalid))
        (for [row content
              :let [trimmed (into {} (remove (fn [[_ v]] (empty? v))) row)]]
          (st/conform type trimmed st/string-conforming))))))

;(parse "resources/parsers/agency.txt")
;(parse "resources/parsers/routes.txt")
;(parse "resources/parsers/stops.txt")
