(ns hiposfer.kamal.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [environ.core :refer [env]]
            [com.stuartsierra.component :as component]
            [hiposfer.kamal.services.webserver.core :as webserver]
            [hiposfer.kamal.services.routing.core :as routing]))


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

(defn config
  "returns a configuration for the system based on defaults, environment
  variables and custom options. The configuration will be merged in that
  order. Returns a configuration map for starting a system"
  ([] (config {}))
  ([custom-options]
   (let [default  (edn/read-string (slurp "resources/default-config.edn"))
         env-opts {:port    (edn/read-string (env :port))
                   :join?   (edn/read-string (env :join?))
                   :osm_url (env :osm_url)}]
     (into default (remove (comp nil? second)) ;; ignore nil values
                   (concat env-opts custom-options)))))

(defn -main [& args]
  (println "\n\nWelcome to the hiposfer/kamal App")
  (let [port       (edn/read-string (first args))
        overwrites {:port port}
        config     (config overwrites)]
    (component/start (system config))))
