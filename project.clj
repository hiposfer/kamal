(defproject sagacious-woof "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [frechet-dist "0.1.2-SNAPSHOT"]
                 [net.mikera/vectorz-clj "0.37.0"] ; probalbe NECESSARY ...
                 [org.clojure/math.numeric-tower "0.0.4"]] ; NECESSARY?
  :main ^:skip-aot sagacious-woof.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
