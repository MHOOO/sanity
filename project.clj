(defproject de.karolski.sanity "0.1.0"
  :description "Transparent addition of types and assertions by
  infering type of arguments from their name."
  :author "Thomas Karolski"
  :dependencies [[org.clojure/clojure "1.3.0-beta1"]
                 [org.clojure.contrib/macro-utils "1.3.0-alpha4"]
                 [org.clojure.contrib/logging "1.3.0-alpha4"]
                 [org.clojure.contrib/seq "1.3.0-alpha4"]
                 [matchure "0.10.1"]]
  :exclusions [org.clojure/clojure-contrib]
  :dev-dependencies [[com.stuartsierra/lazytest "2.0.0-SNAPSHOT"]]
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"})
