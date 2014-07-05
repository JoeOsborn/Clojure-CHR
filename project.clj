(defproject clojure-chr "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://github.com/JoeOsborn/Clojure-CHR"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2173"]
								 [org.clojure/core.typed "0.2.48"]
                 ]

  :plugins [[lein-cljsbuild "1.0.2"]
						[lein-typed "0.3.4"]
            [com.keminglabs/cljx "0.4.0"]]
  
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "src/clj"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "src/cljs"
                   :rules :cljs}]}

  :source-paths ["src/clj"]
  
  :hooks [cljx.hooks]

  :cljsbuild {
    :builds [{:id "clojure-chr"
              :source-paths ["src/cljs" "src/cljx"]
              :compiler {
                :output-to "web/js/clojure-chr.js"
                :output-dir "web/js/out"
                :optimizations :none
                :source-map true}}]})

