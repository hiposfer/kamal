(defproject hiposfer/kamal "0.21.0"
  :description "An application that provides routing based on external sources and OSM data"
  :url "https://github.com/hiposfer/kamal"
  :license {:name "LGPLv3"
            :url  "https://github.com/hiposfer/kamal/blob/master/LICENSE"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 ;; parse xml lazily
                 [org.clojure/data.xml "0.0.8"]
                 ;; abstraction over ring servers - syntactic sugar
                 [compojure "1.6.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-jetty-adapter "1.7.1" :exclusions [ring/ring-core
                                                               ring/ring-codec
                                                               commons-io
                                                               clj-time
                                                               commons-codec]]
                 [metosin/ring-http-response "0.9.1" :exclusions [ring/ring-core
                                                                  ring/ring-codec
                                                                  commons-io
                                                                  clj-time
                                                                  commons-codec]]
                 ;; http content negotiation - accept header
                 [ring-middleware-accept "2.0.3"]
                 [hiposfer/geojson.specs "0.2.0"]
                 [hiposfer/gtfs.edn "0.2.0"]
                 ;; system builder and resource management
                 [com.stuartsierra/component "0.4.0"]
                 ;; for performance in dijkstra routing
                 [org.teneighty/java-heaps "1.0.0"]
                 ;; for gtfs parsing
                 [org.clojure/data.csv "0.1.4"]
                 [datascript "0.17.1"]
                 ;; fast int maps - for faster dijkstra routing
                 [org.clojure/data.int-map "0.2.4"]
                 ;; for nearest neighbour search
                 [ch.hsr/geohash "1.3.0"]
                 ;; needed for heroku - JVM platform metrics
                 ;; otherwise we get - Exception in thread "heroku-java-metrics-agent" java.lang.NoSuchMethodError: com.fasterxml.jackson.databind.JavaType.isReferenceType()Z
                 [com.fasterxml.jackson.core/jackson-databind "2.9.8"]
                 [org.xerial/sqlite-jdbc "3.20.0"]
                 [seancorfield/next.jdbc "1.0.0-rc1"]]
  ;; preprocessor - env vars are not passed along, so better run manually
  ;; ["trampoline" "run" "-m" "hiposfer.kamal.preprocessor"]}
  :profiles {:dev     {:dependencies [[criterium "0.4.4"]   ;; benchmark
                                      [expound "0.7.2"]
                                      [org.clojure/test.check "0.9.0"] ;; generators
                                      [org.clojure/tools.namespace "0.2.11"]]
                       :plugins      [[jonase/eastwood "0.2.9"]]
                       :eastwood     {:config-files ["resources/eastwood.clj"]}}
             :release {:aot                [hiposfer.kamal.core] ;; compile the entry point and all of its dependencies}
                       :main               hiposfer.kamal.core
                       :uberjar-name       "kamal.jar"
                       :jar-exclusions     [#".*\.gz" #".*\.zip"]
                       :uberjar-exclusions [#".*\.gz" #".*\.zip"]
                       :jvm-opts           ["-Dclojure.compiler.direct-linking=true"]}}
  :test-selectors {:default     (fn [m] (not (some #{:benchmark :integration} (keys m))))
                   :benchmark   :benchmark
                   :integration :integration}
  ;;FIXME: https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :repositories [["releases" {:url           "https://clojars.org/repo"
                              :username      :env/clojars_username
                              :password      :env/clojars_password
                              :sign-releases false}]]
  :deploy-repositories [["releases" :releases]])
