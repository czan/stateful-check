(ns stateful-check.symbolic-values-test
  (:require [clojure.test :refer [deftest is testing]]
            [stateful-check.symbolic-values :as sv]))

(deftest test-lookup-var-representation
  (let [root-var (sv/->RootVar "1")]
    (testing "lookup var"
      (let [lookup-var (sv/->LookupVar root-var :x nil)]
        (testing "printed representation"
          (is (= "(get #<1> :x)" (pr-str lookup-var))))
        (testing "string representation"
          (is (= "(get #<1> :x)" (.toString lookup-var))))))
    (testing "lookup var with not found value"
      (let [lookup-var (sv/->LookupVar root-var :x :not-found)]
        (testing "printed representation"
          (is (= "(get #<1> :x :not-found)" (pr-str lookup-var))))
        (testing "string representation"
          (is (= "(get #<1> :x :not-found)" (.toString lookup-var))))))))

(deftest test-root-var-representation
  (testing "root-var"
    (let [root-var (sv/->RootVar "1")]
      (testing "printed representation"
        (is (= "#<1>" (pr-str root-var))))
      (testing "string representation"
        (is (= "#<1>" (.toString root-var)))))))
