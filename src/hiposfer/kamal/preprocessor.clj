(ns hiposfer.kamal.preprocessor
  (:gen-class)
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [hiposfer.kamal.services.routing.core :as routing]
            [hiposfer.kamal.core :as core]
            [spec-tools.core :as st]
            [clojure.walk :as walk])
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

;; TODO: I think it would be best to have a separate spec for this as well
(defn -main
  "Script for preprocessing OSM and GTFS files into gzip files each with
  a Datascript EDN representation inside"
  [outdir]
  (assert (not (nil? outdir)) "missing output file")
  (timbre/info "preprocessing OSM and GTFS files")
  (let [env-vars    (walk/keywordize-keys (into {} (System/getenv)))
        config      (st/conform! ::core/env env-vars st/string-transformer)]
    (doseq [area (:networks config)]
      (let [value (routing/network (dissoc area :area/edn)) ;; just in case
            ;; osm is mandatory, use its filename !!
            path     (str outdir (filename (:area/osm area)) "edn.bz2")]
        (with-open [w (-> (io/output-stream path)
                          (BZip2CompressorOutputStream.)
                          (io/writer))]
          (binding [*out* w]
            (pr @value)))))))

;example
;(-main "resources/")
