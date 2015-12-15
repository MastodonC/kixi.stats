(defproject kixi/stats "0.1.2-SNAPSHOT"
  :description "A library of statistical transducing functions"
  :url "https://github.com/mastodonc/kixi.stats"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.7.0"]
                             [org.clojure/clojurescript "1.7.170"]
                             [org.clojure/test.check "0.9.0"]
                             [doo "0.1.6"]]
              :plugins [[lein-cljsbuild "1.1.1"]
                        [lein-doo "0.1.6"]]}}
  :cljsbuild
  {:builds
   {:test {:source-paths ["src" "test"]
           :compiler {:output-to "target/main.js"
                      :main 'kixi.stats.test-runner
                      :optimizations :whitespace}}}}
  :aliases
  {"test-cljs" ["doo" "phantom" "test" "once"]})
