(defproject hypobus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "LGPLv3"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [frechet-dist "0.8.0"]
                 ; to create geojson objects
                 [org.clojure/data.json "0.2.6"]
                 ; to send http requests to mapbox
                 [http-kit "2.1.19"]]
  :main ^:skip-aot hypobus.core
  :jvm-opts ["-Xmx2g"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
    :dev {:dependencies [; generate test data
                         [org.clojure/test.check "0.9.0"]
                         ; read test data
                         [org.clojure/data.csv "0.1.3"]
                         ; create charts
                         [incanter/incanter-charts "1.5.5"]
                         ; encode charts to base64
                         [org.clojure/data.codec "0.1.0"]
                         ; profiler
                         ;[com.taoensso/timbre "4.3.1"]
                          ]}})
