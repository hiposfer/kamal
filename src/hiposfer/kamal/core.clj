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
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]
            [clojure.string :as str]))

;; matches strings like SAARLAND_AREA_OSM
(def area-regex #"^([A-Z0-9]+)_AREA_(OSM|GTFS|EDN)$")
(defn- area-id [k] (second (re-find area-regex (name k))))

;; TODO: we should check that the values are strings
;; TODO: osm is mandatory, everything else is optional
(defn- networks
  "takes a map of enviroment variables and returns a sequence of
  maps with the area env vars grouped. An extra key :area/id is
  added to each map for dynamic name processing"
  [envs]
  (let [nets (for [k (keys envs)
                   :let [st (name k)]
                   :when (re-matches area-regex st)]
               k)]
    (for [[id ks] (group-by area-id nets)]
     (conj (select-keys envs ks) [:area/id id]))))
;; example
;(networks {:FRANKFURT_AREA_OSM "foo" :SAARLAND_AREA_GTFS 2})

(defn- simplify
  "takes an environment network map and returns a map with :area as key
   namespace and the name file format as ending. For example: :area/gtfs"
  [networks]
  (reduce (fn [result [k v]]
            (let [[match _ ext] (re-find area-regex (name k))]
              (if-not (some? match)
                (conj result [k v])
                (conj result [(keyword "area" (str/lower-case ext)) v]))))
          {}
          networks))
;example
;(simplify {:FRANKFURT_AREA_GTFS "hello" :area/id "frankfurt"})

(defn reshape-env
  "takes a map of environment variables and returns a map
  suitable for kamal system configuration. Return ::s/invalid
  if no valid networks were found"
  [envs]
  (let [nets          (networks envs)
        without-areas (fn [result area] (apply dissoc result (set (keys area))))
        envs          (reduce without-areas envs nets)]
    (if (empty? nets) ::s/invalid
      (conj envs [:networks (map simplify nets)]))))

(s/def ::USE_FAKE_NETWORK spec/boolean?)
(s/def ::JOIN_THREAD spec/boolean?)
(s/def ::PORT spec/integer?)

(s/def ::env (s/and (s/keys :req-un [::PORT]
                            :opt-un [::JOIN_THREAD ::USE_FAKE_NETWORK])
                    (s/conformer reshape-env)))
;; example
;(st/conform! ::env {:PORT "3000"
;                    :FRANKFURT_AREA_OSM "foo" :SAARLAND_AREA_GTFS 2}
;             st/string-transformer)

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

(defn -main [& args]
  (timbre/info "\n\nWelcome to the hiposfer/kamal App")
  (let [env-vars    (into {} (walk/keywordize-keys (System/getenv)))
        config      (st/conform! ::env env-vars st/string-transformer)]
    (component/start (system config))))
