(ns hiposfer.kamal.parsers.gtfs.reference
  "functionality to understand the mapping created between the gtfs reference
    and their datascript representation"
  (:require [clojure.string :as str]
            [clojure.tools.reader.edn :as edn]
            [hiposfer.kamal.libs.tool :as tool]))

(def gtfs-spec (edn/read-string (slurp "gtfs.edn/reference.edn")))

(def identifiers (for [feed (:feeds gtfs-spec)
                       field (:fields feed)
                       :when (:unique field)]
                   field))

(defn- singularly
  "removes the s at the end of a name"
  [text]
  (cond
    (str/ends-with? text "ies") (str (subs text 0 (- (count text) 3)) "y")
    (str/ends-with? text "s") (subs text 0 (dec (count text)))
    :else text))

(defn- reference?
  "checks if text references a field name based on its content. A reference
  is a field name that ends with the same name as a unique field"
  [text]
  (some (fn [uf] (when (str/ends-with? text uf) uf)) (map :field-name identifiers)))

(defn- gtfs-mapping
  "returns a namespaced keyword that will represent this field in datascript"
  [ns-name field]
  (let [field-name (:field-name field)]
    (cond
      ;; "agency_id" -> :agency/id
      (:unique field)
      (keyword ns-name (last (str/split field-name #"_")))

      ;; "route_short_name" -> :route/short_name
      (str/starts-with? field-name ns-name)
      (keyword ns-name (subs field-name (inc (count ns-name))))

      ;; "trip_route_id" -> :trip/route
      (reference? field-name)
      (keyword ns-name (subs field-name 0 (- (count field-name) (count "_id"))))

      ;; "stop_time_pickup_type" -> :stop_time/pickup_type
      :else (keyword ns-name (:field-name field)))))

(defn- feed-namespace
  [feed]
  (if-let [id (tool/some :unique (:fields feed))]
    (first (str/split (:field-name id) #"_"))
    (singularly (first (str/split (:filename feed) #"\.")))))
;;(feed-namespace (nth (:feeds gtfs-spec) 9))

(def fields
  "returns a sequence of gtfs field data with a :mapping entry.

  This is useful to know exactly which fields are mapped to which keywords in
  Datascript"
  (for [feed (:feeds gtfs-spec)
        :let [ns-name (feed-namespace feed)]
        field (:fields feed)]
    (let [k (gtfs-mapping ns-name field)]
      (assoc field :keyword k :filename (:filename feed)))))

(defn get-mapping
  "given a gtfs field name returns its field data from the gtfs reference with
  an extra :mapping key to know its datascript attribute"
  ([filename field-name]
   (reduce (fn [_ v] (when (and (= filename (:filename v))
                                (= field-name (:field-name v)))
                       (reduced v)))
           nil
           fields))
  ([k]
   (reduce (fn [_ v] (when (= k (:keyword v)) (reduced v)))
           nil
           fields)))
;;(get-mapping "agency.txt" "agency_id")
;;(get-mapping "trips.txt" "route_id")
;;(get-mapping "calendar.txt" "service_id")
;;(get-mapping "calendar_dates.txt" "service_id")
;;(get-mapping :calendar/id)
;;(get-mapping :calendar_date/service)