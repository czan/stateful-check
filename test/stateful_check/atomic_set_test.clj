(ns stateful-check.atomic-set-test
  (:require  [clojure.test :refer :all]
             [clojure.test.check.generators :as gen]
             [stateful-check.core :refer :all]))

;;
;; Implementation
;;

(def global-state (atom #{}))

;;
;; Generative commands
;;

(def add-command
  {:args (fn [_] [gen/nat])
   :command #(swap! global-state conj %)
   :next-state (fn [state [arg] _]
                 (conj (or state #{}) arg))})

(def remove-command
  {:requires (fn [state] (seq state))
   :args (fn [state] [(gen/elements state)])
   :command #(swap! global-state disj %)
   :next-state (fn [state [arg] _]
                 (disj state arg))})

(def contains?-command
  {:requires (fn [state] (seq state))
   :args (fn [state] [(gen/one-of [(gen/elements state) gen/nat])])
   :command #(contains? @global-state %)
   :postcondition (fn [state _ [value] result]
                         (= (contains? state value) result))})

(def empty?-command
  {:command #(empty? @global-state)
   :postcondition (fn [state _ _ result]
                         (= (empty? state) result))})

(def empty-command
  {:command (fn [] (reset! global-state #{}))
   :next-state (fn [state _ _] #{})})

;;
;; Generative specification
;;

(def specification
  {:commands {:add #'add-command
              :remove #'remove-command
              :contains? #'contains?-command
              :empty? #'empty?-command
              :empty #'empty-command}
   :initial-state (constantly #{})
   :setup #(reset! global-state #{})})

(deftest atomic-set-test
  (is (specification-correct? specification)))
