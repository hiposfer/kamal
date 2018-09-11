(ns hiposfer.kamal.core
  "This is the entry point for kamal as a production web server.
  It exposes a single `-main` function that can be called through
  the command line (after uberjar compilation) as java -jar ...

  The main function reads the environment variables and uses them
  to build and run the system configuration

  As a convention we treat UPPERCASE names as environmental variables
  and lowercase names as custom created keywords"
  (:gen-class)
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.core :as webserver]
            [hiposfer.kamal.services.routing.core :as routing]
            [taoensso.timbre :as timbre]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; matches strings like SAARLAND_AREA_OSM
(def area-regex #"^(\w+)_AREA_(\w+)$")
(defn area-id? [k] (re-matches area-regex (name k)))
;; (area-id? :FRANKFURT_AM_MAIN_AREA_GTFS)

(defn edn-area? [k] (str/ends-with? (name k) "EDN"))
;;(edn-area? :Frankfurt_am_Main_AREA_EDN)

(defn prepare-areas
  "takes an environment network map and returns a map with :area as key
   namespace and the name file format as ending. For example: :area/gtfs"
  [m]
  (let [nets (filter (fn [[k]] (re-matches area-regex (name k))) m)]
    (for [[k v] nets]
      (let [[_ aname ext] (re-find area-regex (name k))
            id            (str/lower-case (str/replace aname "_" "-"))
            area-name     (str/replace aname "_" " ")]
        (into {:area/name area-name :area/id id}
              [[(keyword "area" (str/lower-case ext)) v]])))))
;; (preprocess-env {:Frankfurt_am_Main_AREA_GTFS "foo"})

(s/def ::USE_FAKE_NETWORK boolean?)
(s/def ::JOIN_THREAD boolean?)
(s/def ::PORT integer?)

(s/def ::env (s/keys :req-un [::PORT]
                     :opt-un [::JOIN_THREAD ::USE_FAKE_NETWORK]))

(s/def ::areas (s/and #(pos? (count %))
                      (s/map-of area-id? string?)
                      (s/map-of edn-area? string?)))

(defn prepare-env
  [env]
  (let [server  (for [[k v] (select-keys env [:PORT :JOIN_THREAD :USE_FAKE_NETWORK])]
                  (if (not (string? v))
                    [k v]
                    [k (edn/read-string v)]))
        sconfig (into {} server)
        areas   (into {} (filter #(re-matches area-regex (name (key %)))) env)]
    (assert (s/valid? ::areas areas) (s/explain-str ::areas areas))
    (assert (s/valid? ::env sconfig) (s/explain-str ::env sconfig))
    (merge sconfig {:networks (prepare-areas areas)})))

(defn system
  "creates a complete system map using components and the provided
  config"
  [config]
  (component/system-map
    :config config
    :router (component/using (routing/service config)
                             [:config])
    :app (component/using (webserver/service)
                          [:config :router])))

(defn -main
  [& args]
  (timbre/info "\n\nWelcome to the hiposfer/kamal App")
  (let [env    (walk/keywordize-keys (into {} (System/getenv)))
        config (prepare-env env)]
    (component/start (system config))))
