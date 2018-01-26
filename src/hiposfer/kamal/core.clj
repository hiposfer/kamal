(ns hiposfer.kamal.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.core :as webserver]
            [hiposfer.kamal.services.routing.core :as routing]))

(defn system
  "creates a complete system map using components and the provided
  config"
  [config]
  (component/system-map
    :config config
    :router (component/using (routing/service)
                             [:config])
    :app (component/using (webserver/service)
                          [:config :router])))

(defn config
  "returns a configuration for the system based on defaults, environment
  variables and custom options. The configuration will be merged in that
  order. Returns a configuration map for starting a system"
  ([] (config {}))
  ([custom-config]
   (let [defaults    (edn/read-string (slurp "resources/defaults.edn"))
         config-path (System/getenv "CONFIG")
         user-config (if-not config-path {}
                       (edn/read-string (slurp config-path)))]
     (into defaults (remove (comp nil? second)) ;; ignore nil values
                    (concat user-config custom-config)))))

(defn -main [& args]
  (println "\n\nWelcome to the hiposfer/kamal App")
  (let [port        (edn/read-string (System/getenv "PORT"))
        config-path (edn/read-string (first args))
        user-config (if-not config-path {}
                      (edn/read-string (slurp config-path)))
        config      (if (some? port)
                      (config (merge user-config {:port port}))
                      (config user-config))]
    (component/start (system config))))
