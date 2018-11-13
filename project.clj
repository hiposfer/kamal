(defproject hiposfer/kamal "0.20.3"
  :description "An application that provides routing based on external sources and OSM data"
  :url "https://github.com/hiposfer/kamal"
  :license {:name "LGPLv3"
            :url "https://github.com/hiposfer/kamal/blob/master/LICENSE"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.xml "0.0.8"] ; parse xml lazily
                 [compojure "1.6.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-jetty-adapter "1.7.0"]
                 [metosin/ring-http-response "0.9.0"] ;; human names for http codes
                 [ring-middleware-accept "2.0.3"] ;; http accept header
                 [hiposfer/geojson.specs "0.2.0"]
                 [hiposfer/gtfs.edn "0.1.1"]
                 [com.stuartsierra/component "0.3.2"] ;; system builder and resource management
                 [org.teneighty/java-heaps "1.0.0"] ;; for performance in dijkstra routing
                 [org.clojure/data.csv "0.1.4"] ;; for gtfs parsing
                 [datascript "0.16.6"]
                 [org.clojure/data.int-map "0.2.4"] ;; fast int maps
                 [ch.hsr/geohash "1.3.0"]] ;; for nearest neighbour search
  ;; preprocessor - env vars are not passed along, so better run manually
  ;; ["trampoline" "run" "-m" "hiposfer.kamal.preprocessor"]}
  :profiles {:dev {:dependencies [[criterium "0.4.4"]  ;; benchmark
                                  [expound "0.7.1"]
                                  [org.clojure/test.check "0.9.0"] ;; generators
                                  [org.clojure/tools.namespace "0.2.11"]]
                   :plugins [[jonase/eastwood "0.2.9"]]
                   :eastwood {:config-files ["resources/eastwood.clj"]}}
             :release {:aot [hiposfer.kamal.core] ;; compile the entry point and all of its dependencies}
                       :main hiposfer.kamal.core
                       :uberjar-name "kamal.jar"
                       :jar-exclusions [#".*\.gz" #".*\.zip"]
                       :uberjar-exclusions [#".*\.gz" #".*\.zip"]
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :test-selectors {:default (fn [m] (not (some #{:benchmark :integration} (keys m))))
                   :benchmark :benchmark
                   :integration :integration}
  ;;FIXME: https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :repositories [["releases"  {:url      "https://clojars.org/repo"
                               :username :env/clojars_username
                               :password :env/clojars_password
                               :sign-releases false}]]
  :deploy-repositories [["releases"  :releases]])
