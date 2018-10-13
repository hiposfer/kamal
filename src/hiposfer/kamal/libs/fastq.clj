(ns hiposfer.kamal.libs.fastq
  "namespace for hand-optimized queries that are used inside the routing
  algorithm and need to run extremely fast (< 1 ms per query)

  By convention all queries here return Entities"
  (:require [datascript.core :as data]
            [hiposfer.kamal.libs.tool :as tool]
            [clojure.string :as str]
            [hiposfer.kamal.network.algorithms.protocols :as np])
  (:import (java.time LocalDate)))

(defn references
  "returns all entities that reference/point to target-id through
  attribute"
  [network attribute target-id]
  (eduction (map #(data/entity network (:e %)))
            (data/datoms network :avet attribute target-id)))

(defn node-edges
  "takes a network and an entity and returns the successors of that entity.
   Only valid for OSM nodes. Assumes bidirectional links i.e. nodes with
   back-references to entity are also returned

  replaces:
  '[:find ?successors ?node
    :in $ ?id
    :where [?id :node/successors ?successors]
           [?node :node/successors ?id]]

  The previous query takes around 50 milliseconds to finish. This function
  takes around 0.25 milliseconds"
  [entity]
  (let [network   (data/entity-db entity)
        target-id (:db/id entity)]
    (concat (references network :edge/src target-id)
            (eduction (map np/mirror)
                      (references network :edge/dst target-id)))))

(defn stop-successors
  [entity]
  (let [network (data/entity-db entity)]
    (concat (references network :arc/src (:db/id entity))
            (eduction (map np/mirror)
                      (references network :edge/dst (:db/id entity))))))

(defn nearest-nodes
  "returns the nearest node/location to point"
  [network point]
  (eduction (map #(data/entity network (:e %)))
            (data/index-range network :node/location point nil)))

(defn continue-trip
  "returns the :stop_time entity to reach ?dst-id via ?trip

  Returns nil if no trip going to ?dst-id was found

  replaces:
  '[:find ?departure
    :in $ ?dst-id ?trip ?start
    :where [?dst :stop_time/stop ?dst-id]
           [?dst :stop_time/trip ?trip]
           [?dst :stop_time/arrival_time ?seconds]
           [(plus-seconds ?start ?seconds) ?departure]]

   The previous query takes around 50 milliseconds to execute. This function
   takes around 0.22 milliseconds to execute. Depends on :stop_time/trip index"
  [network dst trip]
  (let [?dst-id  (:db/id dst)
        ?trip-id (:db/id trip)]
    (tool/some #(= ?dst-id (:db/id (:stop_time/stop %)))
                (references network :stop_time/trip ?trip-id))))

(defn find-trip
  "Returns a [src dst] :stop_time pair for the next trip between ?src-id
   and ?dst-id departing after ?now.

   Returns nil if no trip was found

  replaces:
  '[:find ?trip ?departure
    :in $ ?src-id ?dst-id ?now ?start
    :where [?src :stop_time/stop ?src-id]
           [?dst :stop_time/stop ?dst-id]
           [?src :stop_time/trip ?trip]
           [?dst :stop_time/trip ?trip]
           [?src :stop_time/departure_time ?amount]
           [(hiposfer.kamal.libs.fastq/plus-seconds ?start ?amount) ?departure]
           [(hiposfer.kamal.libs.fastq/after? ?departure ?now)]]

  The previous query runs in 118 milliseconds. This function takes 4 milliseconds"
  [network day-stops src dst now]
  (let [stop_times (eduction (filter #(contains? day-stops (:e %)))
                             (map #(data/entity network (:e %)))
                             (filter #(> (:stop_time/departure_time %) now))
                             (data/datoms network :avet :stop_time/stop (:db/id src)))]
    (when (not-empty stop_times)
      (let [trip (apply min-key :stop_time/departure_time stop_times)]
        [trip (continue-trip network dst (:stop_time/trip trip))]))))

;; src - 74592
;; dst - 74593
;; now - 37049

(defn- working?
  [^LocalDate date service]
  (let [day  (str/lower-case (str (. date (getDayOfWeek))))
        work (keyword "service" day)] ;; :service/monday
    (pos? (work service)))) ;; 1 or 0 according to gtfs spec

(defn day-stop-times
  "returns a set of stop_times entity ids that are available for date"
  [network ^LocalDate date]
  (let [services (into #{} (comp (map #(data/entity network (:e %)))
                                 (filter #(. date (isBefore (:service/end_date %))))
                                 (filter #(. date (isAfter (:service/start_date %))))
                                 (filter #(working? date %))
                                 (map :db/id))
                           (data/datoms network :avet :service/id))
        trips    (into #{} (comp (filter #(contains? services (:v %)))
                                 (map :e))
                           (data/datoms network :avet :trip/service))]
    (into #{} (comp (filter #(contains? trips (:v %)))
                    (map :e))
              (data/datoms network :avet :stop_time/trip))))


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
