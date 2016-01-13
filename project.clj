(defproject
  astro "0.1.0-SNAPSHOT"
  :description "An astrodynamics library."
  :url "https://github.com/markbastian/astro"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [numerics "0.1.0-SNAPSHOT"]]

  :jar-exclusions [#"\.swp|\.swo|\.DS_Store"]
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-cljsbuild "1.1.2"]
                             [org.clojure/clojurescript "1.7.170"]]}
             :cljs {:plugins [[lein-cljsbuild "11.2"]] }}

  :source-paths ["src/clj" "src/cljc"]

  :clj {:builds [{ :source-paths ["src/clj" "src/cljc" "test"] }]}

  :cljsbuild {:builds [{ :source-paths ["src/cljs" "src/cljc"]
                        :compiler { :output-to "resources/public/js/astro.js"
                                   :optimizations :advanced
                                   :pretty-print true}}]})
