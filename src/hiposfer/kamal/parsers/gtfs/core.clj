(ns hiposfer.kamal.parsers.gtfs.core
  (:require [hiposfer.kamal.network.core :as network]
            [hiposfer.kamal.libs.tool :as tool]
            [hiposfer.kamal.network.graph.protocols :as gp]
            [clojure.data.int-map :as imap]
            [hiposfer.kamal.parsers.gtfs.preprocessor :as pregtfs])
  (:import (java.time DayOfWeek LocalTime Duration LocalDateTime)))

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
    [(:service_id calendar)
     (merge (select-keys calendar [:start_date :end_date])
            {:days (reduce-kv rf #{} (select-keys calendar (keys days)))})]))

;(fuse @(first (:networks (:router hiposfer.kamal.dev/system)))
;       (parsedir "resources/gtfs/"))))

;; detailed diagram of files relations
;; http://tommaps.com/wp-content/uploads/2016/09/gtfs-feed-diagram.png

;; Route Planning in Transportation Networks
;; Overview of the different algorithms and models used
;; - https://arxiv.org/pdf/1504.05140.pdf

(defn start-time
  "computes the start LocalTime of a trip based on the Duration instances
  of stop_times"
  [stop_times]
  (LocalTime/ofSecondOfDay (:arrival_time (first stop_times))))

;; trips is a map of trip_id to a map of
;- :src/arrival_time -> the time it takes the vehicle to arrive this stop
;- :src/departure_time -> the time it takes the vehicle to depart from this
;                         stop
;- :dst/arrival_time -> the time it takes the vehicle to arrive at the next
;                       stop
(defrecord Connection [^Long src ^Long dst trips]
  gp/Link
  (src [_] src)
  (dst [_] dst))
  ;;np/Passage TODO -> should be the route ID, I guess
  ;;(way [_] way))


(defn prepare-connections
  "transforms a stop_times sequence into a sequence of Connections"
  [stop_times]
  (let [begin (:arrival_time (first stop_times))]
    (map (fn [s1 s2]
           (map->Connection
             {:src (:stop_id s1)
              :dst (:stop_id s2)
              :trips {(:trip_id s1)
                      {:src/arrival_time   (- (:arrival_time s1) begin)
                       :src/departure_time (- (:departure_time s1) begin)
                       :dst/arrival_time   (- (:arrival_time s2) begin)}}}))
         stop_times (rest stop_times))))

(defn- route-for-conn
  "takes a connection with a single trip and returns its route id"
  [conn gtfs]
  (let [trip-id  (key (first (:trips conn)))
        route-id (:route_id (get (:trips gtfs) trip-id))]
    route-id))

;; custom merger to avoid conflict with numbers
(defn- connector [res v] (if (map? res) (merge res v) res))

(defn- connections
  "returns a sequence of [stop_id [route_id Connection]]"
  [gtfs]
  (let [trips       (group-by :trip_id (:stop_times gtfs))
        connections (sequence (comp (map val) (map prepare-connections) cat) trips)]
    (for [[stop sconns] (group-by gp/src connections)
          [route conns] (group-by #(route-for-conn % gtfs) sconns)]
      [stop [route (apply merge-with connector conns)]])))

(defn- mapize [k] (map #(vector (k %) (dissoc % k))))
(defn- xnode [stop] (network/map->NodeInfo {:lon (:stop_lon stop)
                                            :lat (:stop_lat stop)
                                            :name (:stop_name stop)}))

(defn hash-gtfs
  "transforms the GTFS calendar, stops, trips and route entries
  into a map structure with the entry id as key and the rest as
  value"
  [gtfs]
  (let [nodes    (into (imap/int-map) (comp (mapize :stop_id)
                                            (tool/map-vals xnode))
                       (:stops gtfs))
        trips    (into {} (mapize :trip_id) (:trips gtfs))
        routes   (into (imap/int-map) (mapize :route_id) (:routes gtfs))
        calendar (into {} (map service (:calendar gtfs)))]
    (assoc gtfs :stops nodes :trips trips
                :routes routes :calendar calendar)))

(comment for a connection store a set of
         {:trip_id 1
          :arrival_wait "duration until the vehicle arrives to this stop for this trip"
          :duration "the time it takes to arrive at the next stop"
          :stop_wait "duration that the vehicle stays in the stop"})

(comment for a trip store
         {:route_id 1
          :start_time "LocalTime: time at which this trip starts"})

(comment
  - if a traversal comes with no trip
    for every trip in the connection
      compute :src/departure_time + :trip/start_time
      if current_time < total_time
        take that trip -> return the {:duration :trip_id}
    otherwise
      there is no trip -> Double/POSITIVE_INFINITY
  - if a traversal comes with a trip_id
    find the trip with that id and return the duration)


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

(defn fuse
  "fuses the content of the gtfs data with the network"
  [network hashed-gtfs]
  (let [conns (into {} (connections hashed-gtfs))
        trips (group-by :trip_id (:stop_times hashed-gtfs))]
    (take 10 trips)))

;(def foo (time (hash-gtfs (pregtfs/parsedir "resources/gtfs/"))))

;(keys foo)
;(take 5 (:routes foo))

;(take 2 (connections foo))

;(prepare-connections (second (first (group-by :trip_id (:stop_times foo)))))

;(.with (LocalDateTime/now)
;       (start-time (second (first (group-by :trip_id (:stop_times foo))))))

;(.plus (.with (LocalDateTime/now)
;              (start-time (second (first (group-by :trip_id (:stop_times foo))))))
;       (Duration/ofHours 16))
