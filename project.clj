(defproject hypobus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "LGPLv3"
            :url "https://github.com/carocad/hypobus/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [frechet-dist "0.11.1"] ; compute similarity between curves
                 [org.clojure/data.json "0.2.6"] ; create geojson objects
                 [org.clojure/data.xml "0.0.8"] ; parse xml lazily
                 [org.clojure/data.int-map "0.2.2"] ; fast integers-map
                 [org.clojure/data.priority-map "0.0.7"] ; for dijkstra algorithm
                 [http-kit "2.1.19"]] ; to send http requests to mapbox
  ;; Sets the values of global vars within Clojure.
  :global-vars {*warn-on-reflection* true
                *print-length* 50}
  ;;https://github.com/technomancy/leiningen/issues/2173
  :monkeypatch-clojure-test false
  :plugins [[jonase/eastwood "0.2.3"]]
  :jvm-opts ["-Xmx2g" "-XX:-OmitStackTraceInFastThrow"]
                      ;; prevents NullPointerException   [trace missing]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"] ; generate test data
                                  [org.clojure/data.csv "0.1.3"]]}}) ; read test data
