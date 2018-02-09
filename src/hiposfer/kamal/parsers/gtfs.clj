(ns hiposfer.kamal.parsers.gtfs
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]
            [clojure.string :as str]
            [hiposfer.kamal.specs.gtfs :as gtfs]))

;; agencies
(s/def ::agency_id spec/integer?)
(s/def ::agency (s/keys :req-un [::gtfs/agency_name ::gtfs/agency_timezone]
                        :opt-un [::agency_id]))

;; routes
(s/def ::route_id spec/integer?)
(s/def ::route_type spec/integer?)
(s/def ::route (s/keys :req-un [::route_id ::gtfs/route_short_name ::route_type]
                       :opt-un [::agency_id]))

;; stops
(s/def ::stop_lon (s/and spec/number? ::gtfs/stop_lon))
(s/def ::stop_lat (s/and spec/number? ::gtfs/stop_lat))
(s/def ::stop_id  spec/integer?)
(s/def ::stop (s/keys :req-un [::stop_lon ::stop_lat ::stop_id ::gtfs/stop_name]))

;; trips
(s/def ::trip_id spec/integer?)
(s/def ::trip (s/keys :req-un [::gtfs/route_id ::gtfs/service_id ::trip_id]))

;; stop_times
;; HH:MM:SS
(defn time? [text] (re-matches #"\d{2}:\d{2}:\d{2}" text))

(s/def ::trip_id spec/integer?)
(s/def ::stop_sequence spec/integer?)
(s/def ::stop_time (s/keys :req-un [::trip_id ::gtfs/arrival_time ::gtfs/departure_time
                                    ::stop_id ::stop_sequence]))

;; calendar
;; little hack to transform string integer into booleans
(s/def ::day  (s/and spec/integer? (s/conformer pos?)))
(s/def ::monday    ::day)
(s/def ::tuesday   ::day)
(s/def ::wednesday ::day)
(s/def ::thursday  ::day)
(s/def ::friday    ::day)
(s/def ::saturday  ::day)
(s/def ::sunday    ::day)
(s/def ::calendar (s/keys :req-un [::gtfs/service_id ::monday ::tuesday
                                   ::wednesday ::thursday ::friday
                                   ::saturday ::sunday ::gtfs/start_date
                                   ::gtfs/end_date]))

(def types ;; not all filename correspond to a type so we map them here
  {"agency.txt"   ::agency
   "calendar.txt" ::calendar
   "routes.txt"   ::route
   ;;"shapes.txt" ::gtfs/shapes ;;TODO
   "stop_times.txt" ::stop_time
   "stops.txt"    ::stop
   "trips.txt"    ::trip})

(defn parse
  "takes a filename and parses its content if supported by this parser"
  [filename]
  (with-open [file (io/reader filename)]
    (let [type    (get types (last (str/split filename #"/")))
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
  (into {}
    (for [name (keys types)
          :let [keyname (keyword (first (str/split name #"\.")))]]
      [keyname (parse (str dirname name))])))

;(parse "resources/gtfs/trips.txt")
;(parsedir "resources/gtfs/")

(defn fuse
  [network gtfs]
  (let [stops (:stops gtfs)]
    stops))

(fuse @(first (:networks (:router hiposfer.kamal.dev/system)))
       (parsedir "resources/gtfs/"))
;(take 3 (:graph @(first (:networks (:router hiposfer.kamal.dev/system)))))