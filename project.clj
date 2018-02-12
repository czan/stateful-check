(defproject org.clojars.czan/stateful-check "0.4.0-SNAPSHOT"
  :description "Stateful generative testing in clojure"
  :url "https://github.com/czan/stateful-check"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]]
  :test-selectors {:default #(not (or (:slow %)
                                      (:interactive %)))
                   :interactive :interactive
                   :slow :slow})
