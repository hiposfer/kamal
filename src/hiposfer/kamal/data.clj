(ns hiposfer.kamal.data
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [datascript.core :as data]
            [taoensso.timbre :as timbre]
            [hiposfer.kamal.core :as core])
  (:import (java.util.zip ZipOutputStream ZipEntry ZipInputStream)
           (java.time ZoneId LocalDate DayOfWeek)
           (java.io PushbackReader InputStreamReader)))


(defn- write
  [routers filename]
  (with-open [w0 (ZipOutputStream. (io/output-stream filename))
              w  (io/writer w0)]
    (let [entry (ZipEntry. "datascript.edn")]
      (-> w0 (.putNextEntry entry))
      (binding [*out* w]
        (pr "foo")))));@(first @(:networks (:router system))))))))

(def java-readers
  {'java.time.ZoneRegion #(ZoneId/of %)
   'java.time.LocalDate  #(LocalDate/parse %)
   'java.time.DayOfWeek  #(DayOfWeek/valueOf %)})

(defn read-object
  [[package _ value]]
  (let [parse (get java-readers package)]
    (parse value)))

(defn- read
  [filename]
  (with-open [r0 (ZipInputStream. (io/input-stream filename))
              r  (PushbackReader. (InputStreamReader. r0))]
    (.getNextEntry r0)
    (binding [*read-eval* false]
      (edn/read {:readers (merge data/data-readers
                                 {'hiposfer.kamal.network.core.Location hiposfer.kamal.network.core/map->Location
                                  'object read-object})}
                r))))

(defn -main [[outfile]]
  (timbre/info "\n\npreprocessing OSM and GTFS files")
  (let [config (core/config)]
    (read "resources/datascript.edn")))


;(data/q '[:find (pull ?id [*])
;          :where [?id :service/id]
;          [_ :service/days ?days]]
;        foo)
