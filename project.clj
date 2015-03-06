(defproject com.zentrope/canvas "1"
  :description "Play around with canvas stuff."
  :url "https://zentrope.com"
  :license {:name "epl" :url "http://bit.ly/1EXoLjp"}
  :dependencies
  [[org.clojure/clojure "1.7.0-alpha5"]
   [org.clojure/core.async "0.1.346.0-17112a-alpha"]
   [org.clojure/clojurescript "0.0-2985"]]

  :clean-targets
  ^{:protect false}
  ["resources/www/out"
   "resources/www/main.js"
   :target-path]

  :plugins
  [[lein-cljsbuild "1.0.5"]
   [lein-ancient "0.6.4" :exclusions [org.clojure/clojure]]
   [cider/cider-nrepl "0.8.2"]]

  :cljsbuild
  {:builds [{:id "dev"
             :source-paths ["src"]
             :compiler {:output-to "resources/www/main.js"
                        :output-dir "resources/www/out"
                        :optimizations :none
                        :cache-analysis true
                        :source-map false}}
            {:id "release"
             :source-paths ["src"]
             :compiler {:output-to "resources/www/main.js"
                        :pretty-print true
                        :optimizations :whitespace}}]}

  :profiles
  {:uberjar {:aot :all}})
