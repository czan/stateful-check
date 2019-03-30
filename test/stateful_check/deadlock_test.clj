(ns stateful-check.deadlock-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

(def alloc
  {:command #(Object.)
   :next-state (fn [objects _ new-object]
                 (conj objects new-object))})

(def lock-two-objects
  {:requires #(>= (count %) 2)
   :args (juxt gen/elements gen/elements)
   :precondition #(apply not= %2)
   :command #(locking %1
               (locking %2
                 (Thread/sleep 1)))})

(def deadlock-spec
  {:commands {:alloc #'alloc
              :lock-two-objects #'lock-two-objects}})

(deftest object-locking-passes-sequentially
  (is (specification-correct? deadlock-spec)))

(deftest object-locking-fails-concurrently-with-timeout
  (is (not (specification-correct? deadlock-spec {:gen {:threads 2}
                                                  :run {:max-tries 100
                                                        :timeout-ms 50}}))))
