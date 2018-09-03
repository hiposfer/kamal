(ns hiposfer.kamal.parsers.edn
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [datascript.core :as data]
            [hiposfer.kamal.network.core :as network])
  (:import (java.time LocalDateTime DayOfWeek LocalDate ZoneId)
           (java.io PushbackReader InputStreamReader)
           (java.util.zip GZIPInputStream)))

(def java-readers
  "java class to parsing function. Useful to deserialize things from EDN"
  {'java.time.ZoneRegion    #(ZoneId/of %)
   'java.time.LocalDate     #(LocalDate/parse %)
   'java.time.DayOfWeek     #(DayOfWeek/valueOf %)
   'java.time.LocalDateTime #(LocalDateTime/parse %)})

(defn read-object
  "get the appropiate reader for the Java object and parse it. Throws a
  Null Pointer Exception on unknown class by design !"
  [[package _ value]]
  (let [reader (get java-readers package)]
    (reader value)))

(def local-readers
  "qualified symbol to parsing function. Used to get datascript to understand local objects"
  {'hiposfer.kamal.network.core.Location network/map->Location
   'object                               read-object})

(defn parse
  "streams a zip file and creates a Datascript DB from each file inside the zip.
  Returns a Datascript Database"
  [filename]
  (with-open [stream (-> (io/input-stream filename)
                         (GZIPInputStream.)
                         (InputStreamReader.)
                         (PushbackReader.))]
    (edn/read {:readers (merge data/data-readers local-readers)}
              stream)))

; example
;(time (last (parse "/Users/Camilo/Proyectos/kamal/resources/2122204427.gzip")))
