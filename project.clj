(defproject org.clojars.czan/stateful-check "0.4.0-SNAPSHOT"
  :description "Stateful generative testing in clojure"
  :url "https://github.com/czan/stateful-check"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]]
  :test-selectors {:default (complement :slow)
                   :slow :slow}
  :plugins [[jonase/eastwood "0.2.5"]])
