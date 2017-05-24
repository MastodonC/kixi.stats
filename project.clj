(defproject kixi/stats "0.3.4"
  :description "A library of statistical distribution sampling and transducing functions"
  :url "https://github.com/mastodonc/kixi.stats"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[redux "0.1.3"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.8.0"]
                             [org.clojure/clojurescript "1.9.542"]
                             [org.clojure/test.check "0.9.0"]]
              :plugins [[lein-cljsbuild "1.1.3"]
                        [lein-codox "0.10.3"]
                        [lein-doo "0.1.6"]]}}
  :codox {:project {:name "kixi.stats"}
          :source-uri "https://github.com/MastodonC/kixi.stats/blob/v0.3.4/{filepath}#L{line}"}
  :cljsbuild
  {:builds
   {:test {:source-paths ["src" "test"]
           :compiler {:output-to "target/main.js"
                      :main 'kixi.stats.test-runner
                      :optimizations :whitespace}}}}
  :aliases
  {"test-cljs" ["doo" "phantom" "test" "once"]})
