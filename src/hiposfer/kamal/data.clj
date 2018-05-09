(ns hiposfer.kamal.data
  (:gen-class)
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [hiposfer.kamal.services.routing.core :as routing]
            [hiposfer.kamal.core :as core])
  (:import (java.util.zip GZIPOutputStream)))

(defn- write
  "takes a sequence of Datascript atoms and serializes them to
  a zip"
  [routers outdir]
  (doseq [value routers]
    (let [gfile (str outdir (hash @value) ".gzip")]
      (with-open [w (io/writer (GZIPOutputStream. (io/output-stream gfile)))]
        (binding [*out* w]
          (pr @value))))))

;; "resources/data.zip"
(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  [[outdir]]
  (assert (not (nil? outdir)) "missing output file")
  (timbre/info "\n\npreprocessing OSM and GTFS files")
  (let [config (core/config {:dev false}) ;; just in case
        result (for [area (:networks config)] (routing/network area))]
    (write result outdir)))

;example
;(-main ["resources/"])
