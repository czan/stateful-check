(ns stateful-check.command-runner
  (:require [clojure.walk :as walk]
            [stateful-check.symbolic-values :refer [SymbolicValue get-real-value]]))

(defmulti step-command-runner (fn [state-name & _] state-name))

;; :next-command, :precondition-check, :run-command, :next-state, :postcondition-check
;; :pass, :fail

(defmethod step-command-runner :next-command
  [_ command-list results state]
  (if-let [command-list (next command-list)]
    (let [[sym-var [command & args]] (first command-list)]
      [:precondition-check
       (cons [sym-var [command
                       (walk/prewalk (fn [value]
                                       (if (satisfies? SymbolicValue value)
                                         (get-real-value value results)
                                         value))
                                     args)
                       args]]
             (next command-list))
       results
       state])
    [:pass]))

(defmethod step-command-runner :precondition-check
  [_ [[sym-var [command args]] :as command-list] results state]
  (if-let [precondition (:model/precondition command)]
    (try (if (precondition state args)
           [:run-command
            command-list
            results
            state]
           [:fail])
         (catch Throwable ex
           [:fail ex]))
    [:run-command
     command-list
     results
     state]))

(defmethod step-command-runner :run-command
  [_ [[sym-var [command args]] :as command-list] results state]
  (if-let [real-command (:real/command command)] 
    (try (let [result (apply real-command args)
               results (assoc results sym-var result)]
           [:next-state
            command-list
            results
            state
            result])
         (catch Throwable ex
           [:fail ex]))
    [:fail "No :real/command function!"]))

(defmethod step-command-runner :next-state
  [_ [[sym-var [command args]] :as command-list] results previous-state result]
  (if-let [next-state (or (:real/next-state command)
                          (:next-state command))]
    (try [:postcondition-check
          command-list
          results
          (next-state previous-state args result)
          previous-state
          result]
         (catch Throwable ex
           [:fail ex]))
    [:postcondition-check
     command-list
     results
     previous-state
     previous-state
     result]))

(defmethod step-command-runner :postcondition-check
  [_ [[sym-var [command args]] :as command-list] results next-state prev-state result]
  (if-let [postcondition (:real/postcondition command)]
    (try (if (postcondition prev-state next-state args result)
           [:next-command
            command-list
            results
            next-state]
           [:fail])
         (catch Throwable ex
           [:fail ex]))
    [:next-command
     command-list
     results
     next-state]))

(defmethod step-command-runner :fail [& _])
(defmethod step-command-runner :pass [& _])

(defn run-commands [command-list initial-results initial-state]
  (->> [:next-command (cons nil command-list) initial-results initial-state]
       (iterate (partial apply step-command-runner))
       (take-while (complement nil?))))

(defn passed? [command-results]
  (or (empty? command-results)
      (->> command-results
           (some (comp #{:pass} first))
           boolean)))

(defn extract-exception [command-results]
  (let [failure (->> command-results
                     (filter (comp #{:fail} first))
                     first)]
    (second failure)))

(defn extract-states [command-results]
  (->> command-results
       (filter (comp #{:postcondition-check} first))
       (map #(nth % 3))))
