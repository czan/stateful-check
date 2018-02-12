(ns stateful-check.exception
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

(def counter (atom 0))

(def inc-command
  {:args (fn [state]
           [gen/int])
   :command #(swap! counter + %)
   :next-state (fn [state [arg] _]
                 (+ state arg))})

(def throw-command
  {:command #(when (= @counter 13)
               (throw (RuntimeException. "I don't like 13!")))})

(def spec
  {:commands {:inc #'inc-command
              :throw #'throw-command}
   :initial-state (constantly 0)
   :setup (fn [] (reset! counter 0))
   :cleanup (fn [_] (reset! counter nil))})

(deftest throw-test
  (is (not (specification-correct? spec {:gen {:max-size 20}
                                         :run {:num-tests 500}})))
  (is (= @counter nil)))
