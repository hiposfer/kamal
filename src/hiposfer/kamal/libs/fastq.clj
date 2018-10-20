(ns hiposfer.kamal.libs.fastq
  "namespace for hand-optimized queries that are used inside the routing
  algorithm and need to run extremely fast (< 1 ms per query)

  By convention all queries here return Entities"
  (:require [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import (java.time LocalDate)))

(defn references
  "returns all entities that reference/point to target-id through
  attribute"
  [network attribute target-id]
  (eduction (map #(data/entity network (:e %)))
            (data/datoms network :avet attribute target-id)))

(defn nearest-nodes
  "returns the nearest node/location to point"
  [network point]
  (eduction (map #(data/entity network (:e %)))
            (data/index-range network :node/location point nil)))

(defn- continue-frequency-trip
  "returns a map representation of the cost of reaching ?dst-id through
  a frequency-trip computed previous(ly) as defined in frequencies.txt

  Returns nil if no trip going to ?dst-id was found"
  [network previous ?dst-id]
  (let [from     (:stop_time/to previous)
        ?trip-id (:db/id (:stop_time/trip from))
        to       (tool/some #(= ?dst-id (:db/id (:stop_time/stop %)))
                             (references network :stop_time/trip ?trip-id))]
    (when (some? to)
      (merge previous
             {:value (+ (:value previous)
                        (- (:stop_time/arrival_time to)
                           (:stop_time/departure_time from)))
              :stop_time/from from
              :stop_time/to   to}))))

(defn- continue-fixed-time-trip
  "returns a map representation of the cost of reaching ?dst-id for the
   fixed-time trip previous(ly) found as defined in stop_times.txt.

  Returns nil if no trip going to ?dst-id was found"
  [network previous ?dst-id]
  (let [from     (:stop_time/to previous) ;; the previous to is the new from
        ?trip-id (:db/id (:stop_time/trip from))
        to       (tool/some #(= ?dst-id (:db/id (:stop_time/stop %)))
                             (references network :stop_time/trip ?trip-id))]
    (when (some? to)
      {:value (:stop_time/arrival_time to)
       :stop_time/from from
       :stop_time/to to})))

(defn continue-trip
  "given the previous(ly) found result through (find-trips ...) compute the
  cost of reaching ?dst-id"
  [network previous ?dst-id]
  (if (some? (:frequency/cycle previous))
    (continue-frequency-trip network previous ?dst-id)
    (continue-fixed-time-trip network previous ?dst-id)))

(defn- frequency-cycle
  "given a frequency entity, the duration between two stops on that trip
  and the current time (now), calculate the cycle and arrival time
  such that the user can reach the target stop at value.

  Returns a {:frequency/entity :frequency/cycle :value} map"
  ([frequency duration now]
   (frequency-cycle frequency duration now 1))
  ([frequency duration now n]
   (let [arrival (+ duration (* n (:frequency/headway_secs frequency)))]
     (if (< now arrival)
       {:value arrival :frequency/cycle n :frequency/entity frequency}
       (recur frequency duration now (inc n))))))

(defn- frequency-context
  "returns a sequence of [start src dst] stop_times entities for the given
  ?trip-id, which should be a frequency-based trip"
  [network ?trip-id ?src-id ?dst-id]
  (let [datoms (data/datoms network :avet :stop_time/trip ?trip-id)
        start  (data/entity network (:e (first datoms)))]
    ;; TODO: is there any guarantee that the first datom is the start of the trip ?
    (cons start
      (for [d datoms
            :let [stop_time (data/entity network (:e d))]
            :when (or (= ?src-id (:db/id (:stop_time/stop stop_time)))
                      (= ?dst-id (:db/id (:stop_time/stop stop_time))))]
        stop_time))))

(defn- frequency-matches
  "returns a sequence of

   {:value :stop_times/from :stop_times/to :frequency/cycle :frequency/entity}

   for each frequency-based trip that the user could take to
   travel from ?src-id to ?dst-id at time now"
  [network trips ?src-id ?dst-id now]
  (for [?trip-id trips
        d        (data/datoms network :avet :frequency/trip ?trip-id)
        :let [frequency (data/entity network (:e d))]
        :when (> now (:frequency/start_time frequency))
        :when (< now (:frequency/end_time frequency))
        :let [[start from to] (frequency-context network ?trip-id ?src-id ?dst-id)
              duration  (- (:stop_time/arrival_time to)
                           (:stop_time/departure_time start))]]
    (merge (frequency-cycle frequency duration now)
           {:stop_time/from from
            :stop_time/to to})))

(defn- stop-times-matches
  [network trips ?src-id ?dst-id now]
  (when (seq trips)
    (let [stop_times (eduction (map #(data/entity network (:e %)))
                               (filter #(contains? trips (:db/id (:stop_time/trip %))))
                               (filter #(> (:stop_time/departure_time %) now))
                               (data/datoms network :avet :stop_time/stop ?src-id))]
      (when (seq stop_times)
        (let [from (apply min-key :stop_time/departure_time stop_times)
              to   (tool/some #(= ?dst-id (:db/id (:stop_time/stop %)))
                               (references network :stop_time/trip (:db/id (:stop_time/trip from))))]
          (when (some? to)
            {:value          (:stop_time/arrival_time to)
             :stop_time/from from
             :stop_time/to   to}))))))

(defn find-trip
  "Returns a map with {:value :stop_times/from :stop_times/to} and maybe
   {:frequency/cycle :frequency/entity} on a frequency match.

   To find a matching trip, we search among repetitive trips (frequencies.txt)
   and exact time trips (stop_times.txt). The closest departing trip to now is
   chosen.

   Returns nil if no trip was found"
  [network trips ?src-id ?dst-id now]
  (let [cycle-trips      (frequency-matches network trips ?src-id ?dst-id now)
        frequency-ids    (eduction (map :frequency/entity)
                                   (map :frequency/trip)
                                   (map :db/id)
                                   cycle-trips)
        ;; constraint the available trips to only fixed time
        trips            (set/intersection trips (set frequency-ids))
        fixed-time-trips (stop-times-matches network trips ?src-id ?dst-id now)]
    (cond
      (and (seq cycle-trips) (nil? fixed-time-trips))
      (apply min-key :value cycle-trips)

      (and (empty? cycle-trips) (seq fixed-time-trips))
      (apply min-key :value fixed-time-trips)

      :else
      (apply min-key :value (concat cycle-trips fixed-time-trips)))))

;; src - [:stop/id 3392140086]
;; dst - [:stop/id 582939269]
;; now - 37049

(defn- working?
  [^LocalDate date service]
  (let [day  (str/lower-case (str (. date (getDayOfWeek))))
        work (keyword "service" day)] ;; :service/monday
    (pos? (work service)))) ;; 1 or 0 according to gtfs spec

(defn day-trips
  "returns a set of stop_times entity ids that are available for date"
  [network ^LocalDate date]
  (set (for [service-datom (data/datoms network :avet :service/id)
             :let [service (data/entity network (:e service-datom))]
             :when (and (. date (isBefore (:service/end_date service)))
                        (. date (isAfter (:service/start_date service)))
                        (working? date service))
             trip-datom (data/datoms network :avet :trip/service (:e service-datom))]
         (:e trip-datom))))

;; This might not be the best approach but it gets the job done for the time being
(defn link-stops
  "takes a network, looks up the nearest node for each stop and returns
  a transaction that will link those"
  [network]
  (for [stop (map #(data/entity network (:e %))
                   (data/datoms network :aevt :stop/id))]
    (let [node (first (nearest-nodes network [(:stop/lon stop) (:stop/lat stop)]))]
      (if (not (some? node))
        (throw (ex-info "stop didnt match to any known node in the OSM data"
                        (into {} stop)))
        {:edge/src [:node/id (:node/id node)]
         :edge/dst [:stop/id (:stop/id stop)]}))))

(defn cache-stop-successors
  "computes the next-stops for each stop and returns a transaction
  that will cache those results inside the :stop entities"
  [network]
  (distinct
    (for [trip (data/datoms network :aevt :trip/id)
          :let [refs (references network :stop_time/trip (:e trip))
                stop_times (sort-by :stop_time/stop_sequence refs)]
          [from to] (map vector stop_times (rest stop_times))]
      {:arc/src [:stop/id (:stop/id (:stop_time/stop from))]
       :arc/dst [:stop/id (:stop/id (:stop_time/stop to))]
       :arc/route [:route/id (:route/id (:trip/route (:stop_time/trip from)))]})))
