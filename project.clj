(defproject hiposfer/kamal "0.12.0"
  :description "An application that provides routing based on external sources and OSM data"
  :url "https://github.com/hiposfer/kamal"
  :license {:name "LGPLv3"
            :url "https://github.com/hiposfer/kamal/blob/master/LICENSE"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.xml "0.0.8"] ; parse xml lazily
                 [org.clojure/test.check "0.9.0"] ;; generators
                 [compojure "1.6.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [metosin/ring-http-response "0.9.0"]
                 [ring-middleware-accept "2.0.3"]
                 [metosin/spec-tools "0.7.0"] ;;coercion
                 [hiposfer/geojson.specs "0.2.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [org.teneighty/java-heaps "1.0.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [datascript "0.16.6"]
                 [ch.hsr/geohash "1.3.0"]]
  :aliases {"preprocess" ["trampoline" "run" "-m" "hiposfer.kamal.preprocessor"]}
  :profiles {:dev {:dependencies [[criterium "0.4.4"]  ;; benchmark
                                  [expound "0.7.0"]
                                  [markdown2clj "0.1.3"]
                                  [org.clojure/tools.namespace "0.2.11"]]
                   :plugins [[jonase/eastwood "0.2.6"]]
                   :eastwood {:config-files ["resources/eastwood.clj"]}}
             :release {:aot [hiposfer.kamal.core] ;; compile the entry point and all of its dependencies}
                       :main hiposfer.kamal.core
                       :uberjar-name "kamal.jar"
                       :jar-exclusions [#".*\.gzip" #".*\.zip"]
                       :uberjar-exclusions [#".*\.gzip" #".*\.zip"]
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  ;;FIXME: https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :repositories [["releases"  {:url      "https://clojars.org/repo"
                               :username :env/clojars_username
                               :password :env/clojars_password
                               :sign-releases false}]]
  :deploy-repositories [["releases"  :releases]])
