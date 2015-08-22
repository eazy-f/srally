(defproject srally "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.6"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]]
  :cljsbuild {
    :builds [
      {:source-paths ["src-cljs"]
       :compiler {
;           :output-to "resources/public/srally.js"
           :output-dir "resources/public/"}}]})
