(ns hiposfer.kamal.dev
  "Tools for interactive development with the REPL.

   WARNING:
     This file should NOT be included in a production build of the application

   WARNNING:
     using (refresh!) will fail if you just ran `lein uberjar`
     without cleaning afterwards.
     https://stackoverflow.com/questions/44246924/clojure-tools-namespace-refresh-fails-with-no-namespace-foo"
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.core :as core]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha]
            [clojure.tools.namespace.repl :as repl]
            [taoensso.timbre :as timbre]
            [spec-tools.core :as st]))

(def env
  "a fake environment variables setting for development"
  {:USE_FAKE_NETWORK false
   :JOIN_THREAD false
   :PORT 3000
   :SAARLAND_AREA_GTFS "resources/saarland.gtfs.zip"
   :SAARLAND_AREA_OSM "resources/saarland.min.osm.bz2"
   :SAARLAND_AREA_EDN "resources/saarland.edn.bz2"})

(defonce system nil)

(defn init!
  "Constructs the current development system."
  []
  (alter-var-root #'system
    (constantly (core/system (st/conform! ::core/env env)))))

(defn start!
  "Starts the current development system."
  []
  (timbre/debug "Starting System")
  (alter-var-root #'system component/start))

(defn stop!
  "Shuts down and destroys the current development system."
  []
  (timbre/debug "Stopping System\n")
  (alter-var-root #'system (fn [s] (when s (component/stop s)))))

(defn go!
  "Initializes the current development system and starts it running."
  []
  (stop!)
  (init!)
  (clojure.spec.test.alpha/instrument)
  (set! s/*explain-out* (expound/custom-printer {:theme :figwheel-theme
                                                 :print-specs? false}))
  (start!))

(defn refresh!
  "reset the system to a fresh state. Prefer using this over go!"
  []
  (stop!)
  (repl/refresh :after 'hiposfer.kamal.dev/go!))

;(refresh!)

;(type @(first @(:networks (:router system))))
;(take 10 @(first @(:networks (:router system))))
