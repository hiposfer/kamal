(ns hiposfer.kamal.parsers.gtfs.core
  "namespace for parsing a GTFS feed into a Datascript transaction.

  The algorithms coerces GTFS csv files into entities that reference
  each other through IDs. The IDs are taken directly from GTFS.

  A small dynamic renaming is performed to make it more compatible
  with Datascript design. The renaming is based on the GTFS naming
  convention, for example: stop_lon, stop_url, trip_id, trip_headsign, etc.

  We transform those strings into: :stop/lon, :stop/url, :trip/id,
  :trip/headsign.

  This is important to be able to create relationships between entities through
  transactions, for example {:trip/id 1, :trip/route [:route/id 1]}. This
  allows us to create relationships between entities without actually having
  them, i.e. thread them simply as data"
  (:require [hiposfer.kamal.parsers.gtfs.preprocessor :as pregtfs]
            [clojure.string :as str]
            [hiposfer.kamal.network.core :as network])
  (:import (java.time DayOfWeek)
           (java.util.zip ZipInputStream)))

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

;; set of keywords that are guarantee to define an identifiable element
;; according to GTFS spec
(def ns-keys #{:agency :service :route :stop :trip})
(def links (into #{} (map #(keyword (name %) "id") ns-keys)))
;; map of filename to namespaces. Needed to transform the GTFS entries
;; to Datascript datoms
(def file-ns {"agency.txt"     :agency
              "calendar.txt"   :service
              "routes.txt"     :route
              "trips.txt"      :trip
              "stops.txt"      :stop
              "stop_times.txt" :stop.times})

(def preparer
  "map of GTFS filenames to post-processing functions. Useful to remove
   unnecessary information from the GTFS feed; just to reduce the amount
    of datoms in datascript"
  {"calendar.txt" service
   "routes.txt"   #(dissoc % :route_url)
   "trips.txt"    #(dissoc % :shape_id)
   "stops.txt"    (fn [stop]
                    (let [removable [:stop_lat :stop_lon]
                          ks (apply disj (set (keys stop)) removable)
                          loc (network/->Location (:stop_lon stop) (:stop_lat stop))]
                      (assoc (select-keys stop ks) :stop_location loc)))})

(defn respace
  "takes a keyword and a set of keywords and attempts to convert it into a
  namespaced keyword using the set possibilities. Returns a qualified keyword
  with the matched ns"
  [k]
  (let [nk (name k)]
    (when-let [kt (first (filter #(str/starts-with? nk (name %)) ns-keys))]
      (let [nsk (name kt)]
        (keyword nsk (str/replace-first nk (re-pattern (str nsk "_")) ""))))))
;; example
;(respace :stop_id ns-keys)

(defn- link
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

;; TODO: we can process files this way because Clojure array-maps
;; maintains the order of the elements. However this is only valid for
;; up to 8 key-value pairs. Once we start processing more, we would need
;; to change the logic
(defn datomize!
  "takes a map of gtfs key-name -> content and returns a sequence
  of maps ready to be used for transact"
  [^ZipInputStream zipstream]
  (loop [entry (.getNextEntry zipstream)
         result file-ns] ;; use file-ns array-map order
    (cond
      ;; nothing more to process return
      (nil? entry)
      (sequence cat (vals result))
      ;; unknown file, ignore it
      (not (contains? file-ns (.getName entry)))
      (recur (.getNextEntry zipstream) result)
      ;; important file -> parse and process it
      :else
      (let [filename (.getName entry)
            ns-base  (get file-ns filename)
            prepare  (get preparer filename)
            content  (pregtfs/parse! zipstream filename prepare)
            ;; transform each entry into a valid datascript
            ;; transaction map
            clean    (for [m content] (into {} (map link m (repeat ns-base))))]
        (recur (.getNextEntry zipstream)
               (assoc result filename clean))))))

;(with-open [z (ZipFile. "resources/gtfs/saarland.zip")]
;  (time (last (datomize z))))
