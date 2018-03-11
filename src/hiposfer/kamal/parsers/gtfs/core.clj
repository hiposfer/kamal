(ns hiposfer.kamal.parsers.gtfs.core
  (:require [hiposfer.kamal.parsers.gtfs.preprocessor :as pregtfs]
            [clojure.string :as str]
            [hiposfer.kamal.network.core :as network])
  (:import (java.time DayOfWeek)))

;; detailed diagram of files relations
;; http://tommaps.com/wp-content/uploads/2016/09/gtfs-feed-diagram.png

;; Route Planning in Transportation Networks
;; Overview of the different algorithms and models used
;; - https://arxiv.org/pdf/1504.05140.pdf

;; A good introduction to time-dependent model for timetable information
;; - https://www.ceid.upatras.gr/webpages/faculty/zaro/pub/jou/J23-TTI-Springer.pdf

;; -------------------

;; We will simply have a base penalty whenever you transfer from
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
  namespaced keyword using the set possibilities. Returns a qualified keyword
  with the matched ns"
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
        trips    (map #(dissoc % :shape_id) (:shapes gtfs))
        stops    (for [stop (:stops gtfs)]
                   (let [ks (disj (into #{} (keys stop)) :stop_lat :stop_lon)]
                     (assoc (select-keys stop ks)
                       :stop_location (network/->Location (:stop_lon stop)
                                                          (:stop_lat stop)))))]
    (assoc gtfs :calendar calendar :routes routes :trips trips
                :stops stops)))

;; TODO: deduplicate strings
(defn datomize
  "takes a map of gtfs key-name -> content and returns a sequence
  of maps ready to be used for transact"
  [dirname]
  (for [[k content] (prepare (pregtfs/parsedir dirname))
        m content]
    (into {} (map link m (repeat (k file-keys))))))

;(def foo (time (datomize "resources/gtfs/")))
