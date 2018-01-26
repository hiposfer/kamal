(defproject hiposfer/kamal "0.3.3"
  :description "An application that provides routing based on external sources and OSM data"
  :url "https://github.com/hiposfer/kamal"
  :license {:name "LGPLv3"
            :url "https://github.com/hiposfer/kamal/blob/master/LICENSE"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.xml "0.0.8"] ; parse xml lazily
                 [org.clojure/data.int-map "0.2.4"] ; fast integers-map
                 [org.clojure/test.check "0.9.0"] ;; generators
                 [metosin/compojure-api "2.0.0-alpha18"]
                 [metosin/spec-tools "0.5.1"]
                 [hiposfer/geojson.specs "0.2.0"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [com.stuartsierra/component "0.3.2"]
                 [org.apache.commons/commons-compress "1.4"] ;;bz2 files read
                 [org.clojure/data.avl "0.0.17"]
                 [org.teneighty/java-heaps "1.0.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [ch.hsr/geohash "1.3.0"]]
  :profiles {:dev {:dependencies [[criterium "0.4.4"]  ;; benchmark
                                  [expound "0.1.1"]
                                  [io.aviso/pretty "0.1.34"]
                                  [org.clojure/tools.namespace "0.2.11"]]
                   :plugins [[jonase/eastwood "0.2.3"]
                             [io.aviso/pretty "0.1.34"]]}
             :uberjar {:aot [hiposfer.kamal.core] ;; compile the entry point and all of its dependencies}
                       :main hiposfer.kamal.core
                       :uberjar-name "kamal.jar"}}
                       ;:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :jar-exclusions [#".*\.bz2"]
  :uberjar-exclusions [#".*\.bz2"]
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :main hiposfer.kamal.core
  :global-vars {*warn-on-reflection* true}
  ;;FIXME: https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :jvm-opts ["-Xmx1g" "-XX:-OmitStackTraceInFastThrow"]
  :repositories [["releases"  {:url      "https://clojars.org/repo"
                               :username :env/clojars_username
                               :password :env/clojars_password
                               :sign-releases false}]]
  :deploy-repositories [["releases"  :releases]])
  ;; "-Dclojure.compiler.direct-linking=true"
  ;; https://github.com/clojure/clojure/blob/master/changes.md#11-direct-linking
