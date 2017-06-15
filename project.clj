(defproject backend.routing "0.1.0"
  :description "An application that provides routing services based on external sources and OSM data"
  :url "http://example.com/FIXME"
  :license {:name "LGPLv3"
            :url "https://github.com/carocad/backend/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/data.xml "0.0.8"] ; parse xml lazily
                 [org.clojure/data.int-map "0.2.2"]] ; fast integers-map
  ;; Sets the values of global vars within Clojure.
  :global-vars {*warn-on-reflection* true
                *print-length* 50}
  ;;FIXME: https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :plugins [[jonase/eastwood "0.2.3"]]
  :jvm-opts ["-Xmx4g"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}) ; generate test data
