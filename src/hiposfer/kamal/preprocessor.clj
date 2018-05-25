(ns hiposfer.kamal.preprocessor
  (:gen-class)
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [hiposfer.kamal.services.routing.core :as routing]
            [hiposfer.kamal.core :as core])
  (:import (org.apache.commons.compress.compressors.bzip2 BZip2CompressorOutputStream)
           (java.net MalformedURLException)))

(defn- filename
  "try to extract a filename from the provided string"
  [text]
  (try
    (let [url ^java.net.URL (io/as-url text)
          conn (.openConnection url)
          content (-> conn (.getHeaderField "Content-Disposition"))]
      (second (re-find #"filename\*=utf-8''(\w+)" content)))
    (catch MalformedURLException _
      (second (re-find #"\/(\w+)\." text)))))

;; "resources/data.zip"
(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  [outdir]
  (assert (not (nil? outdir)) "missing output file")
  (timbre/info "preprocessing OSM and GTFS files")
  (let [config (core/config {:dev false})] ;; just in case
    (doseq [area (:networks config)]
      (let [value (routing/network area)
            ;; osm is mandatory, use it !!
            path     (str outdir (filename (:osm area)) ".bz2")]
        (with-open [w (-> (io/output-stream path)
                          (BZip2CompressorOutputStream.)
                          (io/writer))]
          (binding [*out* w]
            (pr @value)))))))

;example
;(-main ["resources/"])
