(defproject org.n7a235/service.routing "0.1.0"
  :description "An application that provides routing services based on external sources and OSM data"
  :url "http://example.com/FIXME"
  :license {:name "LGPLv3"
            :url "https://github.com/carocad/service/blob/master/LICENSE"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/data.xml "0.0.8"] ; parse xml lazily
                 [org.clojure/data.int-map "0.2.4"] ; fast integers-map
                 [metosin/compojure-api "2.0.0-alpha7"]
                 ;;[metosin/spec-tools "0.3.2"]
                 [metosin/spec-tools "0.3.3-SNAPSHOT"]
                 [org.clojure/test.check "0.9.0"]]
  :profiles {:dev {:dependencies [[criterium "0.4.4"]  ;; benchmark
                                  [expound "0.1.1"]
                                  [io.aviso/pretty "0.1.34"]]}}
  :uberjar-name "routing.jar"
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :global-vars {*warn-on-reflection* true}
  ;;FIXME: https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :plugins [[jonase/eastwood "0.2.3"]
            [lein-ring "0.12.0"]
            [io.aviso/pretty "0.1.34"]]
  :ring {:handler service.routing.core/app
         :auto-reload? true}
  :jvm-opts ["-Xmx500m" "-XX:-OmitStackTraceInFastThrow"])
  ;; "-Dclojure.compiler.direct-linking=true"
  ;; https://github.com/clojure/clojure/blob/master/changes.md#11-direct-linking
