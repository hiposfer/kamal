(ns service.routing.dev
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application."
  (:require [com.stuartsierra.component :as component]
            [clojure.edn :as edn]
            [environ.core :refer [env]]
            [service.routing.core :as routing]
            [clojure.tools.namespace.repl :as repl]))

;; todo: why not use an atom for this?
(def system nil)

(defn init!
  "Constructs the current development system."
  []
  (alter-var-root #'system
    (constantly (routing/system {:port (edn/read-string (env :port))
                                 :join? (edn/read-string (env :join?))}))))

(defn start!
  "Starts the current development system."
  []
  (alter-var-root #'system component/start))

(defn stop!
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'system (fn [s] (when s (component/stop s)))))

(defn go!
  "Initializes the current development system and starts it running."
  []
  (when system ;; prevent leaking resources if already started
    (stop!))
  (init!)
  (start!))

;; WARN: this will fail if you just ran `lein uberjar` without
;; cleaning afterwards. See
;; https://stackoverflow.com/questions/44246924/clojure-tools-namespace-refresh-fails-with-no-namespace-foo
(defn reset
  "reset the system to a fresh state. Prefer using this over go!"
  []
  (stop!)
  (repl/refresh :after 'service.routing.dev/go!))

;(reset)
