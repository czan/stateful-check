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

(defn push-queue-with-race-condition [queue val]
  (let [q @queue]
    ;; look, a race condition!
    ;; this is equivalent to (swap! queue conj val)
    ;; except with a race condition
    (reset! queue (conj q val)))
  nil)

(defn peek-queue [queue]
  (peek @queue))

(defn pop-queue [queue]
  (let [value @queue]
    (if (compare-and-set! queue value (pop value))
      (peek value)
      (recur queue))))

(defn count-queue [queue]
  (count @queue))

(defn count-queue-constantly-zero [queue]
  0)

;;
;; Generative testing commands
;;

(def push-queue-command
  {:args (fn [state] [(:queue state) gen/nat])
   :command #'push-queue
   :next-state (fn [state [_ val] _]
                 (update-in state [:elements] conj val))
   :postcondition (fn [_ _ _ result]
                         (nil? result))})

(def push-queue-command-with-race-condition
  (assoc push-queue-command :command #'push-queue-with-race-condition))

(def peek-queue-command
  {:args (fn [state] [(:queue state)])
   :precondition (fn [state _] (seq (:elements state)))
   :command #'peek-queue
   :postcondition (fn [state _ args val]
                         (= val (first (:elements state))))})

(def pop-queue-command
  {:requires (fn [state] (seq (:elements state)))
   :args (fn [state] [(:queue state)])
   :command #'pop-queue
   :next-state (fn [state _ _]
                 (update-in state [:elements] (comp vec next)))
   :postcondition (fn [state _ args val]
                         (= val (first (:elements state))))})

(def count-queue-command
  {:args (fn [state] [(:queue state)])
   :command #'count-queue
   :postcondition (fn [state _ _ val]
                         (= val (count (:elements state))))})

(def count-queue-constantly-zero-command
  (assoc count-queue-command :command #'count-queue-constantly-zero))

;;
;; Generative testing specification
;;

(def queues-in-use (atom 0))

(def queue-specification
  {:commands {:push #'push-queue-command
              :peek #'peek-queue-command
              :pop #'pop-queue-command
              :count #'count-queue-command}
   :initial-state (fn [queue]
                    {:queue queue,
                     :elements []})
   :setup (fn []
                 (swap! queues-in-use inc)
                 (new-queue))
   :cleanup (fn [state]
                   (swap! queues-in-use dec))})

(def failing-queue-specification
  (assoc-in queue-specification
            [:commands :count] #'count-queue-constantly-zero-command))

(def parallel-failing-queue-specification
  (assoc-in queue-specification
            [:commands :push] #'push-queue-command-with-race-condition))

(deftest correct-queue-test
  (let [val @queues-in-use]
    (is (specification-correct? queue-specification))
    (is (specification-correct? queue-specification {:gen {:threads 2
                                                           :max-length 10}
                                                     :run {:max-tries 10}}))
    (is (= val @queues-in-use) "setup/cleanup should both be run for all tests (pass and fail)")))

(deftest failing-queue-test
  (let [val @queues-in-use]
    (is (not (specification-correct? failing-queue-specification)))
    (is (= val @queues-in-use) "setup/cleanup should both be run for all tests (pass and fail)")))

(deftest parallel-failing-queue-test
  (let [val @queues-in-use]
    (is (specification-correct? parallel-failing-queue-specification))
    (is (not (specification-correct? parallel-failing-queue-specification {:gen {:threads 2}
                                                                           :run {:max-tries 10}})))
    (is (= val @queues-in-use) "setup/cleanup should both be run for all tests (pass and fail)")))
