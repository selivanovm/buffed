(defproject ui "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3126"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [racehub/om-bootstrap "0.4.1"]
                 [org.omcljs/om "0.8.8"]
                 [cljs-ajax "0.3.9"]]

  :plugins [[lein-cljsbuild "1.0.5"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "ui"
              :source-paths ["src"]
              :compiler {
                :output-to "ui.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
