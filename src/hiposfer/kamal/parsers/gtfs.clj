(ns hiposfer.kamal.parsers.gtfs
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]
            [clojure.string :as str]
            [hiposfer.kamal.specs.gtfs :as gtfs]
            [hiposfer.kamal.network.core :as network]
            [java-time :as jt]))

;(System/getProperty "java.version")

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
(s/def ::trip (s/keys :req-un [::route_id ::gtfs/service_id ::trip_id]))

;; stop_times
;; HH:MM:SS
(defn time? [text] (re-matches #"\d{2}:\d{2}:\d{2}" text))

(s/def ::trip_id spec/integer?)
(s/def ::stop_sequence spec/integer?)
(s/def ::arrival_time (s/conformer jt/local-time))
(s/def ::departure_time (s/conformer jt/local-time))
(s/def ::stop_time (s/keys :req-un [::trip_id ::arrival_time ::departure_time
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
  "takes a filename and parses its content if supported by this parser.
   Entries that do not conform to the gtfs spec are removed. Returns
   a vector of conformed entries"
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
  (into {} (map (fn [name] [(keyword (first (str/split name #"\.")))
                            (parse (str dirname name))]))
           (keys types)))
;(parse "resources/gtfs/trips.txt")
;(def foo (parsedir "resources/gtfs/"))
;(take 10 (:stop_times foo))
;(take 5 (:trips foo))

(defn- node-entry
  "convert a gtfs stop into a node"
  [stop]
  [(:stop_id stop)
   (network/map->NodeInfo {:lon (:stop_lon stop) :lat (:stop_lat stop)
                           :name (:stop_name stop)})])

(defn fuse
  "fuses the content of the gtfs data with the network"
  [network gtfs]
  (let [stops (:stops gtfs)
        nodes (map node-entry stops)]
    (assoc network :graph (into (:graph network) nodes))))

;(fuse @(first (:networks (:router hiposfer.kamal.dev/system)))
;       (parsedir "resources/gtfs/"))))

;(take 5 (:stops (parsedir "resources/gtfs/")))
;(take 3 (:graph @(first (:networks (:router hiposfer.kamal.dev/system)))))

;; detailed diagram of files relations
;; http://tommaps.com/wp-content/uploads/2016/09/gtfs-feed-diagram.png

;; Route Planning in Transportation Networks
;; Overview of the different algorithms and models used
;; https://arxiv.org/pdf/1504.05140.pdf

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
;; - for each consecutive stops_id in a trip
;;   - create a time dependent arc (this represents the trip between two stops)
;;   - gather all trips departing from the src node
;;     - create a function that given a time will return the time until the
;;       next stop, the route id and the trip id
;; - for each stop_sequence 0 on a trip
;;   - create a time dependent arc (This is a fake/transition arc)
;;   - create a function that given a time will compute the time to wait
;;     until the next vehicle

;(jt/local-time "05:00:00")