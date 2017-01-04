(defproject kixi/stats "0.3.2"
  :description "A library of statistical transducing functions"
  :url "https://github.com/mastodonc/kixi.stats"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[redux "0.1.3"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.8.0"]
                             [org.clojure/clojurescript "1.8.40"]
                             [org.clojure/test.check "0.9.0"]]
              :plugins [[lein-cljsbuild "1.1.3"]
                        [lein-codox "0.9.4"]
                        [lein-doo "0.1.6"]]}}
  :codox {:namespaces [kixi.stats.core]
          :project {:name "kixi.stats"}
          :source-uri "https://github.com/MastodonC/kixi.stats/blob/v0.3.2/{filepath}#L{line}"}
  :cljsbuild
  {:builds
   {:test {:source-paths ["src" "test"]
           :compiler {:output-to "target/main.js"
                      :main 'kixi.stats.test-runner
                      :optimizations :whitespace}}}}
  :aliases
  {"test-cljs" ["doo" "phantom" "test" "once"]})
