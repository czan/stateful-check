(ns stateful-check.queue-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

;;
;; Simple mutable queue implementation
;;

(defn new-queue []
  (atom (clojure.lang.PersistentQueue/EMPTY)))

(defn push-queue [queue val]
  (swap! queue conj val)
  nil)

(defn peek-queue [queue]
  (peek @queue))

(defn pop-queue [queue]
  (let [val (peek @queue)]
    (swap! queue pop)
    val))

(defn count-queue [queue]
  (count @queue))

;;
;; Generative testing commands
;;

(def push-queue-command
  {:model/args (fn [state] [(:queue state) gen/nat])
   :real/command #'push-queue
   :next-state (fn [state [_ val] _]
                 (update-in state [:elements] conj val))})

(def peek-queue-command
  {:model/args (fn [state] [(:queue state)])
   :model/precondition (fn [state _] (seq (:elements state)))
   :real/command #'peek-queue
   :real/postcondition (fn [state _ args val]
                         (= val (first (:elements state))))})

(def pop-queue-command
  {:model/args (fn [state] [(:queue state)])
   :model/precondition (fn [state _] (seq (:elements state)))
   :real/command #'pop-queue
   :next-state (fn [state _ _]
                 (update-in state [:elements] (comp vec next)))})

(def count-queue-command
  {:model/args (fn [state] [(:queue state)])
   :real/command #'count-queue
   :real/postcondition (fn [state _ _ val]
                         (= val (count (:elements state))))})

;;
;; Generative testing specification
;;

(def queue-specification
  {:commands {:push #'push-queue-command
              :peek #'peek-queue-command
              :pop #'pop-queue-command
              :count #'count-queue-command}
   :initial-state (fn [queue] {:queue queue, :elements []})
   :real/setup #'new-queue})

(deftest queue-test
  (is (specification-correct? queue-specification)))
