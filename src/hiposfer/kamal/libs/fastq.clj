(ns hiposfer.kamal.libs.fastq
  "namespace for hand-optimized queries that are used inside the routing
  algorithm and need to run extremely fast (< 1 ms per query)"
  (:require [datascript.core :as data])
  (:import (java.time LocalDateTime LocalDate LocalTime)))

(defn node-successors
  "takes a network and an entity id and returns the successors of that entity.
   Only valid for OSM nodes. Assumes bidirectional links i.e. nodes with
   back-references to id are also returned

  replaces:
  '[:find ?successors ?node
    :in $ ?id
    :where [?id :node/successors ?successors]
           [?node :node/successors ?id]]

  The previous query takes around 50 milliseconds to finish. This function
  takes around 0.25 milliseconds"
  [network id]
  (concat (map :db/id (:node/successors (data/entity network id)))
          (eduction (map :e) (take-while #(= (:v %) id))
                    (data/index-range network :node/successors id nil))))

(defn nearest-node
  "returns the nearest node/location datom to point"
  [network point]
  (first (data/index-range network :node/location point nil)))

(defn node-ways
  "takes a dereferenced Datascript connection and an entity id and returns
  the OSM ways that reference it. Only valid for OSM node ids

  replaces:
  '[:find ?way
    :in $ ?id
    :where [?way :way/nodes ?id]]

  The previous query takes around 50 milliseconds to finish. This one takes
  around 0.15 milliseconds"
  [network id]
  (take-while #(= (:v %) id) (data/index-range network :way/nodes id nil)))

;; utility functions to avoid Datascript complaining about it
(defn- plus-seconds [^LocalDateTime t amount] (.plusSeconds t amount))
(defn- after? [^LocalDateTime t ^LocalDateTime t2] (.isAfter t t2))

(defn- continue-xform
  "for some reason, defining the transducer outside of the eduction itself
  seems to be faster"
  [network ?dst-id ?trip-id ?start]
  (comp (take-while #(= (:v %) ?trip-id))
        (map #(data/entity network (:e %)))
        (filter #(= ?dst-id (:db/id (:stop.times/stop %))))
        (map :stop.times/arrival_time)
        (map #(plus-seconds ?start %))))

(defn- continue-trip
  "find the time of arrival for a certain trip to a specifc stop

  replaces:
  '[:find ?departure
    :in $ ?dst-id ?trip ?start
    :where [?dst :stop.times/stop ?dst-id]
           [?dst :stop.times/trip ?trip]
           [?dst :stop.times/arrival_time ?seconds]
           [(plus-seconds ?start ?seconds) ?departure]]

   The previous query takes around 50 milliseconds to execute. This function
   takes around 0.22 milliseconds to execute. Depends on :stop.times/trip index.
   "
  [network ?dst-id ?trip ?start]
  (eduction (continue-xform network ?dst-id ?trip ?start)
            (data/index-range network :stop.times/trip ?trip nil)))


(defn- xf
  "reducing function for upcoming-trip. Just for convenience"
  [res v]
  (if (pos? (compare (:stop.times/arrival_time res) (:stop.times/arrival_time v)))
    v
    res))

(defn- close-range
  "convenience function to get the entities whose attribute (k) equals id"
  [network k id]
  (eduction (take-while #(= (:v %) id))
            (map #(data/entity network (:e %)))
            (data/index-range network k id nil)))

(defn- upcoming-xform
  "returns a transducer to get only the stop.times entries that are part of the same
  trip containing both src and dst and whose arrival time is after the current
  user time"
  [four ?start ?now]
  (comp (filter #(contains? four (:stop.times/trip %)))
        (filter #(after? (plus-seconds ?start (:stop.times/departure_time %)) ?now))))

;; TODO: ideally these computations can be cached to improve performance
;; to avoid cache misses I guess the ideal way would be to cache the trip
;; retrieval from the DB since it is what takes most of the time. See below.
;; Therefore we could add a System Component which exposes some memoized functions
;; with fancy algorithms like LRU or LU ;)
;; such that a query like (trips network from-id to-id) can be performed quickly

;"Elapsed time: 0.048362 msecs" -> sources
;"Elapsed time: 0.023676 msecs" -> dests
;"Elapsed time: 0.607559 msecs" -> four
;"Elapsed time: 3.154331 msecs" -> five -> (into [] (upcoming-xform four ?start ?now) sources))
;"Elapsed time: 0.296584 msecs" -> stime
;"Elapsed time: 0.004389 msecs" -> result

(defn- upcoming-trip
  "replaces:
  '[:find ?trip ?departure
    :in $ ?src-id ?dst-id ?now ?start
    :where [?src :stop.times/stop ?src-id]
           [?dst :stop.times/stop ?dst-id]
           [?src :stop.times/trip ?trip]
           [?dst :stop.times/trip ?trip]
           [?src :stop.times/arrival_time ?amount]
           [(hiposfer.kamal.libs.fastq/plus-seconds ?start ?amount) ?departure]
           [(hiposfer.kamal.libs.fastq/after? ?departure ?now)]]

  The previous query runs in 118 milliseconds. This function takes 4 milliseconds"
  [network ?src-id ?dst-id ?start ?now]
  (let [sources (close-range network :stop.times/stop ?src-id)
        dests   (close-range network :stop.times/stop ?dst-id)
        four    (into {} (map #(vector (:stop.times/trip %) %)) dests)
        stime   (reduce xf (upcoming-xform four ?start ?now) sources)]
    [stime  (get four (:stop.times/trip stime))]))

;(time)
;(upcoming-trip @(first @(:networks (:router hiposfer.kamal.dev/system)))
;               230963
;               230607
;               (LocalDateTime/now)
;               (LocalDateTime/of (LocalDate/now) (LocalTime/MIDNIGHT)))

;(data/pull @(first @(:networks (:router hiposfer.kamal.dev/system))) '[*] 213810)

;(data/datoms @(first @(:networks (:router hiposfer.kamal.dev/system)))
;             :avet :stop/id)

;(data/entity @(first @(:networks (:router hiposfer.kamal.dev/system))) [:stop/id 263906742])
;(data/entity @(first @(:networks (:router hiposfer.kamal.dev/system))) [:stop/id 5069917764])

;(time)
;  (link-stops @(first @(:networks (:router hiposfer.kamal.dev/system)))))
;
;(let [a (data/transact! (first @(:networks (:router hiposfer.kamal.dev/system)))
;                        (link-stops @(first @(:networks (:router hiposfer.kamal.dev/system)))))]
;  (println "OK"))
;
;(into {} (data/entity @(first @(:networks (:router hiposfer.kamal.dev/system)))
;                      403908))