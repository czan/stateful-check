(ns stateful-check.mutation-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

(def system-under-test (atom nil))

(def add-command
  {:command #(swap! system-under-test conj 1)
   :next-state (fn [expect _ _]
                 (conj expect 1))})

(def observe-command
  {:command #(do system-under-test)
   :postcondition (fn [expect _ _ result]
                    (= expect @result))})

(def mutation-spec
  {:commands {:add #'add-command
              :observe #'observe-command}
   :initial-state (constantly [])
   :setup #(reset! system-under-test [])})

(deftest ^:interactive catches-mutation
  ;; This test is marked as "interactive" because we want to see the
  ;; output of it. The test should fail, but check to see whether you
  ;; get a warning about an object being mutated. You should see
  ;; something indicating that the object was mutated later in the
  ;; test, and showing the most recent value for it.
  (is (specification-correct? mutation-spec)))
