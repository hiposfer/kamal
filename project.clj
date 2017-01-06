(defproject hypobus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "LGPLv3"
            :url "https://github.com/carocad/hypobus/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 ; compute similarity between curves
                 [frechet-dist "0.11.1"]
                 ; create geojson objects
                 [org.clojure/data.json "0.2.6"]
                 ; parse xml lazily
                 [org.clojure/data.xml "0.0.8"]
                 ; fast integers-map
                 [org.clojure/data.int-map "0.2.2"]
                 ; priority map - for dijkstra algorithm
                 [org.clojure/data.priority-map "0.0.7"]
                 ; to send http requests to mapbox
                 [http-kit "2.1.19"]]
  ;; Sets the values of global vars within Clojure.
  :global-vars {*warn-on-reflection* true}
  :plugins [[jonase/eastwood "0.2.3"]]
  :jvm-opts ["-Xmx2g"]
  :profiles {:dev {:dependencies [; generate test data
                                  [org.clojure/test.check "0.9.0"]
                                  ; read test data
                                  [org.clojure/data.csv "0.1.3"]]}})
