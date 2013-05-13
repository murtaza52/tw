(defproject tw "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/algo.generic "0.1.1"]
                 [swiss-arrows "0.6.0"]
                 [clj-time "0.5.0"]
                 [table "0.4.0"]]
  :plugins [[lein-midje "3.0.0"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}}
  :repl-options {:init-ns tw.core
                 :init (do
                         (use 'tw.core)
                         (assemble-tracks))})
