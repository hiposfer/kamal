(defproject sagacious-woof "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "LGPLv3"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [frechet-dist "0.5.0"]
                 [net.mikera/vectorz-clj "0.40.0"]
                 [org.clojure/test.check "0.9.0"]

                 [incanter/incanter-charts "1.5.5"]
                 [incanter/incanter-core "1.5.5"]]
  :main ^:skip-aot sagacious-woof.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
