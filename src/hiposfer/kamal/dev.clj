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
            [clojure.tools.namespace.repl :as repl]))

(def env
  "a fake environment variables setting for development"
  {:USE_FAKE_NETWORK false
   :JOIN_THREAD false
   :PORT 6000
   :FRANKFURT_AM_MAIN_AREA_EDN "resources/test/frankfurt.edn.gz"})

(defonce system nil)

(defn init!
  "Constructs the current development system."
  []
  (alter-var-root #'system
    (constantly (core/system (core/prepare-env env)))))

(defn start!
  "Starts the current development system."
  []
  (println "Starting System")
  (alter-var-root #'system component/start))

(defn stop!
  "Shuts down and destroys the current development system."
  []
  (println "Stopping System\n")
  (alter-var-root #'system (fn [s] (when s (component/stop s)))))

(defn custom-printer
  [explain-data]
  (let [printer (expound/custom-printer {:show-valid-values? true
                                         :print-specs? false})]
    (printer explain-data)))

(defn go!
  "Initializes the current development system and starts it running."
  []
  (stop!)
  (init!)
  (clojure.spec.test.alpha/instrument)
  (alter-var-root #'s/*explain-out* (constantly custom-printer))
  (set! *warn-on-reflection* true)
  (set! *print-length* 50)
  (start!))

(defn refresh!
  "reset the system to a fresh state. Prefer using this over go!"
  []
  (stop!)
  (repl/refresh :after 'hiposfer.kamal.dev/go!))

;(refresh!)

;(type @(first @(:networks (:router system))))
;(take 10 @(first @(:networks (:router system))))
