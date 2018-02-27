(ns hiposfer.kamal.parsers.gtfs
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]
            [clojure.string :as str]
            [hiposfer.kamal.specs.gtfs :as gtfs]
            [hiposfer.kamal.network.core :as network])
  (:import (java.time DayOfWeek LocalTime LocalDate ZoneId Duration)
           (java.time.format DateTimeFormatter)))

;; NOTE: In general I like the API of clj-time more
;; However after trying both of them for parsing the date/time
;; in the GTFS files, the clj-time parsing was 10 times slower
;; than the one in java-time. Since the server needs to provide
;; very fast time-dependent routing I will stick with java-time :)

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
(s/def ::stop_id  spec/integer?)
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
        [hh mm ss] (map #(Long/parseLong %) values)]
    (reduce (fn [^Duration res v] (.plus res v))
            (Duration/ofHours hh)
            [(Duration/ofMinutes mm) (Duration/ofSeconds ss)])))

(s/def ::trip_id spec/integer?)
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

(def days {:monday    DayOfWeek/MONDAY
           :tuesday   DayOfWeek/TUESDAY
           :wednesday DayOfWeek/WEDNESDAY
           :thursday  DayOfWeek/THURSDAY
           :friday    DayOfWeek/FRIDAY
           :saturday  DayOfWeek/SATURDAY
           :sunday    DayOfWeek/SUNDAY})

(defn service
  "transforms a calendar entry into a [id Service], where
  Service contains the period and set of days that applies.
  Preferable representation to speed up comparisons"
  [calendar]
  (let [rf       (fn [r k v] (if (true? v) (conj r (k days)) r))]
    (merge (select-keys calendar [:start_date :end_date :service_id])
           {:days (reduce-kv rf #{} (select-keys calendar (keys days)))})))

;; -----------------------------------------------------------------
;(service (second (:calendar foo)))

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

(defn- node-entry
  "convert a gtfs stop into a node"
  [stop]
  [(:stop_id stop)
   (network/map->NodeInfo {:lon (:stop_lon stop)
                           :lat (:stop_lat stop)
                           :name (:stop_name stop)})])

(defn fuse
  "fuses the content of the gtfs data with the network"
  [network gtfs]
  (let [stops      (:stops gtfs)
        nodes      (map node-entry stops)
        calendar   (into {} (map service (:calendar gtfs)))
        stop-pairs (sequence (comp (mapcat (fn [[_ stops]] (partition 2 1 stops)))
                                   (take 10))
                             (group-by :trip_id (:stop_times gtfs)))]
    (assoc network :graph (into (:graph network) nodes))))

;(fuse @(first (:networks (:router hiposfer.kamal.dev/system)))
;       (parsedir "resources/gtfs/"))))

;(take 5 (:stops (parsedir "resources/gtfs/")))
;(take 3 (:graph @(first (:networks (:router hiposfer.kamal.dev/system)))))

;; detailed diagram of files relations
;; http://tommaps.com/wp-content/uploads/2016/09/gtfs-feed-diagram.png

;; Route Planning in Transportation Networks
;; Overview of the different algorithms and models used
;; - https://arxiv.org/pdf/1504.05140.pdf

;; NOTES
;; - A route is a group of trips that are displayed to riders as a single service.
;; - A trip is a sequence of two or more stops that occurs at specific time
;; - a Stop_times entry is a time entry that a vehicle arrives at and departs
;;   from individual stops for each trip
;; - a Stop is an Individual locations where vehicles pick up or drop off passengers

;; Base on the previous information the fusion of network and gtfs can be done as
;; follows:
;; - map the stop ID to node ID
;; - create a :gtfs/routes entry in the network to contain the metadata
;; - group the stop_times by trip_id
;; - for each consecutive stops_id in a trip
;;   - create a [src dst] tuple
;;   - group them by src node
;;     - group those by route_id
;;     - combine/reduce the tuple into a single connection
;;       - this will behave as a time-dependent arc
;;       - it will contain a sorted set with the :src/arrival_time as key
;;       - while traversing a simple (avl/nearest > ...) should be enough
;;         to get the corresponding connection
;;       - neither the stop_id nor the stop_sequence are needed inside the connection

;(def foo (time (parsedir "resources/gtfs/")))

(defn start-time
  "computes the start time of a trip based on the Duration instances
  of stop_times"
  [stop_times]
  (let [first_arrive ^Duration (:arrival_time (first stop_times))
        hh           (.toHours first_arrive)
        mm           (.toMinutes (.minusHours first_arrive hh))
        ss           (.getSeconds (.minusMinutes (.minusHours first_arrive hh)
                                                 mm))]
    (LocalTime/of hh mm ss)))

(defrecord Connection [^Long src ^Long dst route-id trips])

(defn rebase-time
  "re-computes the arrival and departure time of each stop_time based
  on the first stop"
  [stop_times]
  (let [begin (:arrival_time (first stop_times))]
    (for [stop-time stop_times]
      (assoc stop-time :arrival_time (.minus (:arrival_time stop-time) begin)
                       :departure_time (.minus (:departure_time stop-time) begin)))))

;(let [trips (group-by :trip_id (:stop_times foo))]
;  (for [[id trip] (take 10 trips)]
;    (let [start (start-time trip)]
;      {:trip_id id :start_time start})))

;(:calendar foo)
;(take 1 (group-by :route_id (:trips foo)))

