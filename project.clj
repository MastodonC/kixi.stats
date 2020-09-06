(defproject kixi/stats "0.5.4"
  :description "A library of statistical distribution sampling and transducing functions"
  :url "https://github.com/mastodonc/kixi.stats"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[redux "0.1.4"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [com.tdunning/t-digest "3.2"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.10.1"]
                             [org.clojure/clojurescript "1.10.764"]]
              :plugins [[lein-cljsbuild "1.1.8"]
                        [lein-doo "0.1.11"]]}}
  :cljsbuild
  {:builds
   {:test {:source-paths ["src" "test"]
           :compiler {:output-to "target/main.js"
                      :main kixi.stats.test-runner
                      :optimizations :none}}}}
  :aliases
  {"test-cljs" ["doo" "phantom" "test" "once"]})
