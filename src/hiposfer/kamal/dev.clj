(ns hiposfer.kamal.dev
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application."
  (:require [com.stuartsierra.component :as component]
            [hiposfer.kamal.core :as core]
            [clojure.spec.test.alpha :as st]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]
            [clojure.tools.namespace.repl :as repl]
            [taoensso.timbre :as timbre]
            [datascript.core :as data]
            [clojure.edn :as edn])
  (:import (java.io PushbackReader)))

(defonce system nil)

(defn init!
  "Constructs the current development system."
  []
  (alter-var-root #'system
    (constantly (core/system (core/config {:dev false
                                           :join? false})))))

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
  (st/instrument)
  (set! s/*explain-out* (expound/custom-printer {:theme :figwheel-theme
                                                 :print-specs? false}))
  (start!))

;; WARN: this will fail if you just ran `lein uberjar` without
;; cleaning afterwards. See
;; https://stackoverflow.com/questions/44246924/clojure-tools-namespace-refresh-fails-with-no-namespace-foo
(defn refresh!
  "reset the system to a fresh state. Prefer using this over go!"
  []
  (stop!)
  (repl/refresh :after 'hiposfer.kamal.dev/go!))

;(reset)

;(set! *print-length* 50)
;(take 10 (:network @(:network (:grid system))))

;(type @(first @(:networks (:router system))))


;(with-open [w (clojure.java.io/writer "resources/datascript.edn")]
;  (binding [*out* w]
;    (pr @(first @(:networks (:router system))))))
;
;(with-open [r (PushbackReader. (clojure.java.io/reader "resources/datascript.edn"))]
;  (binding [*read-eval* false]
;    (edn/read {:readers (merge data/data-readers
;                               {'hiposfer.kamal.network.core.Location hiposfer.kamal.network.core/map->Location
;                                'object identity})}
;              r)))
