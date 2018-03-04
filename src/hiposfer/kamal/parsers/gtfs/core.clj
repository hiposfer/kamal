(ns hiposfer.kamal.parsers.gtfs.core
  (:require [hiposfer.kamal.network.core :as network]
            [hiposfer.kamal.libs.tool :as tool]
            [hiposfer.kamal.network.graph.protocols :as gp]
            [clojure.data.int-map :as imap]
            [clojure.data.priority-map :as mprio]
            [hiposfer.kamal.parsers.gtfs.preprocessor :as pregtfs]
            [hiposfer.kamal.network.graph.core :as graph]
            [datascript.core :as data]
            [clojure.string :as str]
            [clojure.data.avl :as avl])
  (:import (java.time DayOfWeek)))

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

;; trips is a map of trip_id to a map of
;- :src/arrival_time -> the time it takes the vehicle to arrive this stop
;                       measured from midnight
;- :src/departure_time -> the time it takes the vehicle to depart from this
;                         stop measured from midnight
;- :dst/arrival_time -> the time it takes the vehicle to arrive at the next
;                       stop measured from midnight
(defrecord Connection [^Long src ^Long dst trips]
  gp/Link
  (src [_] src)
  (dst [_] dst))
  ;;np/Passage TODO -> should be the route ID, I guess
  ;;(way [_] way))


(defn prepare-connections
  "transforms a stop_times sequence into a sequence of Connections"
  [stop_times]
  (map (fn [s1 s2]
         (map->Connection
           {:src (:stop_id s1)
            :dst (:stop_id s2)
            :trips (mprio/priority-map-keyfn :src/departure_time
                     (:trip_id s1)
                     {:src/arrival_time   (:arrival_time s1)
                      :src/departure_time (:departure_time s1)
                      :dst/arrival_time   (:arrival_time s2)})}))
       stop_times
       (rest stop_times)))

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
  [gtfs trips]
  (let [connections (sequence (comp (map val)
                                    (map prepare-connections)
                                    cat)
                              trips)]
    (for [[stop sconns] (group-by gp/src connections)
          [route conns] (group-by #(route-for-conn % gtfs) sconns)]
      [stop [route (apply merge-with connector conns)]])))


(defn- mapize [k] (map #(vector (k %) (dissoc % k))))
(defn- xnode [stop] (network/map->NodeInfo {:lon (:stop_lon stop)
                                            :lat (:stop_lat stop)
                                            :name (:stop_name stop)}))

(defn gtfs-by-id
  "transforms the GTFS calendar, stops, trips and route entries
  into a map structure with the entry id as key and the rest as
  value"
  [gtfs]
  (assoc gtfs
    :stops    (into (imap/int-map) (comp (mapize :stop_id) (tool/map-vals xnode))
                    (:stops gtfs))
    :trips    (into {} (mapize :trip_id) (:trips gtfs))
    :routes   (into (imap/int-map) (mapize :route_id) (:routes gtfs))
    :calendar (into {} (map service) (:calendar gtfs))))

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
    - remove the trips that dont drive that day
    - for every trip in the connection
        compute :src/departure_time + :user/date
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

;; detailed diagram of files relations
;; http://tommaps.com/wp-content/uploads/2016/09/gtfs-feed-diagram.png

;; Route Planning in Transportation Networks
;; Overview of the different algorithms and models used
;; - https://arxiv.org/pdf/1504.05140.pdf

;; A good introduction to time-dependent model for timetable information
;; - https://www.ceid.upatras.gr/webpages/faculty/zaro/pub/jou/J23-TTI-Springer.pdf

(defn- node-merger
  "reducer function to merge a graph with a gtfs stops without losing information"
  [graph [id stop]]
  (if (contains? graph id)
    (update graph id merge stop)
    (assoc graph id stop)))

;; NOTE: we wont model the vehicle stations as mini-graph as is proposed in most
;; articles. Although it makes the model clearer from a graph theory perspective
;; it also makes it more difficult to maintain, specially considering real-time
;; updates. For example: to change the transfer time between two routes at a
;; specific station, you would have to find out that station, then find out
;; the arc connecting those and update it. As if that wasnt enough, you need to
;; create fake nodes for the route stops (which id do you use? how do you ensure
;; that it is unique or that no collisions will occur in the future?)

;; Instead of that we will simply have a base penalty whenever you transfer from
;; a route to another. If there is a special connection between two routes then
;; it can be stored in a :gtfs/transfers structure which can be easily lookup
(defn fuse
  "fuses the content of the gtfs data with the network"
  [network mgtfs]
  (let [trips     (group-by :trip_id (:stop_times mgtfs))
        conns     (connections mgtfs trips)
        removable (imap/difference (imap/int-set (keys (:stops mgtfs)))
                                   (imap/int-set (map first conns)))
        stops     (apply dissoc (:stops mgtfs) removable)
        graph     (reduce node-merger (:graph network) stops)]
    ;; TODO: link nodes with routes
    (map second conns)))

(def ns-keys #{:agency :service :route :stop :trip})
(def links (into #{} (map #(keyword (name %) "id") ns-keys)))
(def file-keys {:agency     :agency
                :calendar   :service
                :routes     :route
                :stop_times :stop.times
                :stops      :stop
                :trips      :trip}) ;; TODO shape

(defn respace
  "takes a keyword and a set of keywords and attempts to convert it into a
  namespaced keyword using the set posibilities. Returns a qualified keyword
  with the matched ns or default-ns otherwise"
  [k]
  (let [nk (name k)]
    (when-let [kt (first (filter #(str/starts-with? nk (name %)) ns-keys))]
      (let [nsk (name kt)]
        (keyword nsk (str/replace-first nk (re-pattern (str nsk "_")) ""))))))
;(respace :stop_id ns-keys)

(defn link
  "transforms a [k v] pair into a namespaced version with a possible reference
  to another model. The returned pair is suitable to use in a Datalog
  transaction. Returns the key with the base as namespace otherwise

  Example: (link [:trip_id 123] :trip) => [:trip/id 123]
           (link [:route_id 456] :trip) => [:trip/route [:route/id 456]]"
  [[k v] base]
  (let [nk    (or (respace k) k)
        sbase (name base)]
    (cond
      (= (namespace nk) sbase) [nk v]
      (contains? links nk) [(keyword sbase (namespace nk)) [nk v]]
      :else [(keyword sbase (name nk)) v])))

(def schema {:trip/id {:db.unique :db.unique/identity}
             :trip/route {:db/type :db.type/ref}
             :trip/service {:db/type :db.type/ref}

             :agency/id {:db.unique :db.unique/identity}

             :service/id {:db.unique :db.unique/identity}

             :route/id {:db.unique :db.unique/identity}
             :route/agency {:db/type :db.type/ref}

             :stop.time/trip {:db/type :db.type/ref}
             :stop.time/stop {:db/type :db.type/ref}})

(defn datomize
  "takes a map of gtfs key-name -> content and returns a sequence
  of maps ready to be used for transact"
  [gtfs]
  (for [[k content] gtfs
        m content]
    (into {} (map link m (repeat (k file-keys))))))

;(def foo (time (pregtfs/parsedir "resources/gtfs/")))

;(def conn (data/create-conn schema));(data/transact! conn (:trips foo))
;(data/transact! conn (:stop_times foo))
;(take-last 5 (data/datoms @conn :eavt))
;(data/entity @conn [:trip_id 406014.151])

;(keys foo)
;(take 5 (:trips foo))

;(def network (keys @(first (:networks (:router hiposfer.kamal.dev/system)))))
;(take 2 (fuse network foo))

;(fuse @(first (:networks (:router hiposfer.kamal.dev/system)))
;       (parsedir "resources/gtfs/"))))

;(data/transact! conn (datomize foo))

;(time (data/q '[:find (pull ?route [*]) .
;                :where [?route :route/id 450854]]
;               @conn))

;(take-last 10 (data/datoms @conn :eavt))