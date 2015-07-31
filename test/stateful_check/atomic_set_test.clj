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
  {:model/args (fn [_] [gen/nat])
   :real/command #(swap! global-state conj %)
   :next-state (fn [state [arg] _]
                 (conj (or state #{}) arg))})

(def remove-command
  {:model/requires (fn [state] (seq state))
   :model/args (fn [state] [(gen/elements state)])
   :real/command #(swap! global-state disj %)
   :next-state (fn [state [arg] _]
                 (disj state arg))})

(def contains?-command
  {:model/requires (fn [state] (seq state))
   :model/args (fn [state] [(gen/one-of [(gen/elements state) gen/nat])])
   :real/command #(contains? @global-state %)
   :real/postcondition (fn [state _ [value] result]
                         (= (contains? state value) result))})

(def empty?-command
  {:real/command #(empty? @global-state)
   :real/postcondition (fn [state _ _ result]
                         (= (empty? state) result))})

(def empty-command
  {:real/command (fn [] (reset! global-state #{}))
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
   :real/setup #(reset! global-state #{})})

(deftest atomic-set-test
  (is (specification-correct? specification)))
