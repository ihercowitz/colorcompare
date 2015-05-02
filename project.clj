(defproject colorcompare "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.3.3"]
                 [http-kit  "2.1.16"]
                 [cheshire "5.4.0"]
                 [ring/ring-json "0.3.1"]]

  :main ^:skip-aot colorcompare.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
