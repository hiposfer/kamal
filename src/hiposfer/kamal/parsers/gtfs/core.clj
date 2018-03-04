(ns hiposfer.kamal.parsers.gtfs.core
  (:require [hiposfer.kamal.parsers.gtfs.preprocessor :as pregtfs]
            [clojure.string :as str])
  (:import (java.time DayOfWeek)))

;; -------------------
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

;; -------------------
;; NOTE:
;; we wont model the vehicle stations as mini-graph as is proposed in most
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

(defn prepare
  "remove unnecessary information from the gtfs feed. Just to reduce the
  amount of datoms in datascript"
  [gtfs]
  (let [calendar (map service (:calendar gtfs))
        routes   (map #(dissoc % :route_url) (:routes gtfs))
        trips    (map #(dissoc % :shape_id) (:shapes gtfs))]
    (assoc gtfs :calendar calendar :routes routes :trips trips)))

(defn datomize
  "takes a map of gtfs key-name -> content and returns a sequence
  of maps ready to be used for transact"
  [dirname]
  (for [[k content] (prepare (pregtfs/parsedir dirname))
        m content]
    (into {} (map link m (repeat (k file-keys))))))

;(def foo (time (datomize "resources/gtfs/")))

;(take-last 5 (data/datoms @conn :eavt))
;(data/entity @conn [:trip_id 406014.151])

;(let [a (data/transact! conn foo)]
;  (take-last 5 (data/datoms @conn :eavt)))

;(time (data/q '[:find (pull ?route [*]) .
;                :where [?route :route/id 450854]]
;               @conn))

;(take-last 10 (data/datoms @conn :eavt))
