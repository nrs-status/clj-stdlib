(defproject clj-stdlib "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.1.0"]
                 ]
    :profiles {:dev {:dependencies [[djblue/portal "0.59.2"]]}}
  :repl-options {:init-ns clj-stdlib.user})
