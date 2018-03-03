(ns hiposfer.kamal.parsers.gtfs.preprocessor
  (:require [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [hiposfer.kamal.specs.gtfs :as gtfs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [spec-tools.core :as st])
  (:import (java.time ZoneId Duration LocalDate)
           (java.time.format DateTimeFormatter)))


;(System/getProperty "java.version") > 8

;; agencies
(s/def ::agency_id spec/integer?)
(s/def ::agency_timezone (s/and ::gtfs/agency_timezone
                                (s/conformer #(ZoneId/of %))))
(s/def ::agency (s/keys :req-un [::gtfs/agency_name ::agency_timezone]
                        :opt-un [::agency_id]))

;; routes
(s/def ::route_id spec/integer?)
(s/def ::route_type spec/integer?)
(s/def ::route (s/keys :req-un [::route_id ::gtfs/route_short_name ::route_type]
                       :opt-un [::agency_id]))

;; stops
(s/def ::stop_lon (s/and spec/number? ::gtfs/stop_lon))
(s/def ::stop_lat (s/and spec/number? ::gtfs/stop_lat))
(s/def ::stop_id spec/integer?)
(s/def ::stop (s/keys :req-un [::stop_lon ::stop_lat ::stop_id ::gtfs/stop_name]))

;; trips
(s/def ::trip_id spec/integer?)
(s/def ::trip (s/keys :req-un [::route_id ::gtfs/service_id ::trip_id]))

;; stop_times
(defn- duration
  "parse the arrival and departure time into Duration instances. This is
  due to GTFS allowing times greater than 23:59:59 which is the biggest
  Java Local Time. A Trip real arrival time can then be calculated as the
  start time + duration at each stop"
  [text]
  (let [[_ & values] (re-matches gtfs/re-time text)
        [hh mm ss] (map #(Long/parseLong %) values)
        [hh mm ss] [(Duration/ofHours hh)
                    (Duration/ofMinutes mm)
                    (Duration/ofSeconds ss)]]
    (.getSeconds (reduce (fn [^Duration res v] (.plus res v)) hh [mm ss]))))

(s/def ::trip_id spec/double?)
(s/def ::stop_sequence spec/integer?)
(s/def ::arrival_time (s/and ::gtfs/arrival_time
                             (s/conformer duration)))
(s/def ::departure_time (s/and ::gtfs/departure_time
                               (s/conformer duration)))
(s/def ::stop_time (s/keys :req-un [::trip_id ::arrival_time ::departure_time
                                    ::stop_id ::stop_sequence]))

;; calendar
(def date-format (DateTimeFormatter/ofPattern "uuuuMMdd"))
(defn- local-date [text] (LocalDate/parse text date-format))
;; little hack to transform string integer into booleans
(s/def ::day  (s/and spec/integer? (s/conformer pos?)))
(s/def ::monday    ::day)
(s/def ::tuesday   ::day)
(s/def ::wednesday ::day)
(s/def ::thursday  ::day)
(s/def ::friday    ::day)
(s/def ::saturday  ::day)
(s/def ::sunday    ::day)
(s/def ::start_date (s/and ::gtfs/start_date
                           (s/conformer local-date)))
(s/def ::end_date (s/and ::gtfs/end_date
                         (s/conformer local-date)))
(s/def ::calendar (s/keys :req-un [::gtfs/service_id ::monday ::tuesday
                                   ::wednesday ::thursday ::friday
                                   ::saturday ::sunday ::start_date
                                   ::end_date]))

;; not all filename correspond to a type so we map them here
(def conformers
  {"agency.txt"   ::agency
   "calendar.txt" ::calendar
   "routes.txt"   ::route
   ;;"shapes.txt" ::gtfs/shapes ;;TODO
   "stop_times.txt" ::stop_time
   "stops.txt"    ::stop
   "trips.txt"    ::trip})

(defn parse
  "takes a filename and parses its content if supported by this parser.
   Entries that do not conform to the gtfs spec are removed. Returns
   a vector of conformed entries"
  [filename]
  (with-open [file (io/reader filename)]
    (let [type    (get conformers (last (str/split filename #"/")))
          raw     (csv/read-csv file)
          head    (map keyword (first raw))
          content (map zipmap (repeat head) (rest raw))]
      (into [] (remove #(= % ::s/invalid))
        (for [row content
              :let [trimmed (into {}  (remove #(empty? (second %))) row)]]
          (st/conform type trimmed st/string-conforming))))))

(defn parsedir
  "takes a directory name ending in / and returns a map of
   [entity-keyword content] for the supported types"
  [dirname]
  (into {} (map (fn [name] [(keyword (first (str/split name #"\.")))
                            (parse (str dirname name))]))
           (keys conformers)))