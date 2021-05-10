(ns stateful-check.postcondition-is-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]
            [stateful-check.generator :refer [default-shrink-strategies ]]
            [stateful-check.shrink-strategies :as shrink]
            [stateful-check.queue-test :as q]))

(deftest ^:interactive postcondition-prints-in-parallel-case
  ;; This test is marked as "interactive" because we want to see the
  ;; output of it. The test should fail, but check to see whether you
  ;; can see the postcondition assertion output (expected and actual,
  ;; along with the message).
  (is (specification-correct? (-> q/parallel-failing-queue-specification
                                  (assoc-in [:commands :push :postcondition]
                                            (fn [_ _ _ result] (is (nil? result))))
                                  (assoc-in [:commands :peek :postcondition]
                                            (fn [state _ _ val] (is (= (first (:elements state)) val))))
                                  (assoc-in [:commands :pop :postcondition]
                                            (fn [state _ _ val] (is (= (first (:elements state)) val))))
                                  (assoc-in [:commands :push :count]
                                            (fn [state _ _ val] (is (= (count (:elements state)) val)))))
                              {:gen {:threads 2}
                               :run {:max-tries 10}})))


(deftest ^:interactive failed-assertion-is-printed
  (is (specification-correct?
       {:commands {:cmd {:command       (constantly true)
                         :postcondition (fn [_ _ _ _]
                                          (is (= 1 0)))}}})))

(deftest ^:interactive exception-in-assertion-is-printed
  (is (specification-correct?
       {:commands {:cmd {:command       (constantly true)
                         :postcondition (fn [_ _ _ _]
                                          (is (throw (ex-info "An exception!" {:oh "no"}))))}}})))

(deftest assertion-is-used-instead-of-return-value-is-printed
  (is (specification-correct?
       {:commands {:cmd {:command       (constantly true)
                         :postcondition (fn [_ _ _ _]
                                          (is (= 1 1))
                                          ;; falsey return value, but
                                          ;; ignored becuase of the
                                          ;; above assertion (which
                                          ;; passes)
                                          false)}}})))
