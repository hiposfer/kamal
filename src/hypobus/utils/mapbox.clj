(ns hypobus.utils.mapbox
  "mapbox map matching functions"
  (:require [org.httpkit.client :as http]
            [clojure.data.json :as json]))

(def ^:const ^:private MAX-POINTS 100)

(def ^:private json-template
  {:type "Feature",
   :properties nil,
   :geometry {:type "LineString",
              :coordinates nil}})

(defn- gps-precision
  [poly-line]
  (let [weights    (map :weight)
        deviations (map #(Math/sqrt (/ %)))
        total      (transduce (comp weights deviations) + poly-line)
        avg        (/ total (count poly-line))]
    (int (max 30 (min 50 avg)))))

(defn- ->geo-json
  [curve]
  (assoc-in json-template [:geometry :coordinates]
            (mapv #(vector (:lon %) (:lat %)) curve)))

(defn write-geojson
  [filename curve]
  (with-open [out (clojure.java.io/writer filename)]
    (json/write (->geo-json curve) out)))

;; (defn- ->multigeo-json
;;   [curves]
;;   (assoc-in json-template [:geometry :coordinates]
;;             (mapv (fn [line] (mapv #(vector (:lon %) (:lat %)) line))
;;                   curves)))

;; (defn write-multiline
;;   [filename curves]
;;   (with-open [out (clojure.java.io/writer filename)]
;;     (json/write (->multigeo-json curves) out)))

(defn- match-curve
  [poly-line access-key]
  (let [json-curve (->geo-json poly-line)
        params     (str "access_token=" access-key
                        "&gps_precision=" (gps-precision poly-line))
        profile    "mapbox.driving.json?"
        url        "https://api.mapbox.com/matching/v4/"
        options    {:headers {"Content-Type" "application/json; charset=utf-8"}
                    :body    (json/write-str json-curve)}
        response   (http/post (str url profile params) options)]
    (json/read-str (:body @response) :key-fn keyword)))

(defn map-match
  [poly-line access-key]
  (if (> MAX-POINTS (count poly-line))
    (match-curve poly-line access-key)
    (let [subcurves (partition-all MAX-POINTS poly-line)]
      (map match-curve subcurves (repeat access-key)))))

(defn match->files
  [poly-line filename access-key]
  (let [result (map-match poly-line access-key)]
    (map (fn [part index]
           (with-open [out (clojure.java.io/writer (str filename "-" index ".geojson"))]
             (json/write part out)))
         result (range (count result)))))
