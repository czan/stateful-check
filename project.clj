(defproject org.clojars.czan/stateful-check "0.4.4-SNAPSHOT"
  :description "Stateful generative testing in clojure"
  :url "https://github.com/czan/stateful-check"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "1.1.0"]]
  :test-selectors {:default #(not (:interactive %))
                   :interactive :interactive}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]])
