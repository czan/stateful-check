(ns stateful-check.command-runner
  (:require [clojure.walk :as walk]
            [stateful-check
             [command-utils :as u]
             [symbolic-values :refer [get-real-value SymbolicValue]]]))

(defmulti step-command-runner
  "Step the command runner state machine one step. Each state in the
  state machine is represented by a \"variant\", which is a vector
  with a key (the state name) and a series of values. What work needs
  to be done in each state is taken care of by this method's
  implementations, and they return a new state variant."
  (fn [state-name & _] state-name))

;; :next-command, :run-command, :next-state, :postcondition-check
;; :pass, :fail

(defmethod step-command-runner :next-command
  [_ command-list results state spec]
  (if (seq command-list)
    (try (if (u/check-spec-postcondition spec state)
           (let [[sym-var [command & raw-args]] (first command-list)
                 args (walk/prewalk (fn [value]
                                      (if (satisfies? SymbolicValue value)
                                        (get-real-value value results)
                                        value))
                                    raw-args)]
             [:run-command
              [sym-var [command args raw-args]]
              (next command-list)
              results
              state
              spec])
           [:fail])
         (catch Throwable ex
           [:fail ex]))
    [:pass]))

(defmethod step-command-runner :run-command
  [_ [sym-var [command args raw-args] :as current] command-list results state spec]
  (try (let [result (u/run-command command args)
             results (assoc results sym-var result)]
         [:next-state
          current
          command-list
          results
          state
          result
          spec])
       (catch Throwable ex
         [:fail ex [sym-var [command args raw-args]]])))

(defmethod step-command-runner :next-state
  [_ [sym-var [command args raw-args] :as current] command-list results previous-state result spec]
  (try [:postcondition-check
        current
        command-list
        results
        (u/real-make-next-state command previous-state args result)
        previous-state
        result
        spec]
       (catch Throwable ex
         [:fail ex [sym-var [command args raw-args]]])))

(defmethod step-command-runner :postcondition-check
  [_ [sym-var [command args raw-args] :as current] command-list results next-state prev-state result spec]
  (try (if (u/check-postcondition command prev-state next-state args result)
         [:next-command
          command-list
          results
          next-state
          spec]
         [:fail])
       (catch Throwable ex
         [:fail ex [sym-var [command args raw-args]]])))

;; terminal states, so return `nil`
(defmethod step-command-runner :fail [& _])
(defmethod step-command-runner :pass [& _])

(defn run-commands
  "Run the given list of commands with the provided initial
  results/state. Returns a realized seq of states from the command
  runner."
  [command-list initial-results initial-state spec]
  (->> [:next-command command-list initial-results initial-state spec]
       (iterate (partial apply step-command-runner))
       (take-while (complement nil?))
       doall))

(defn passed?
  "Determine whether a list of command runner states represents a
  successfully completed execution."
  [command-results]
  (or (empty? command-results)
      (->> command-results
           (some (comp #{:pass} first))
           boolean)))

(defn extract-exception
  "Return the exception thrown during the execution of commands for
  this result list."
  [command-results]
  (let [failure (->> command-results
                     (filter (comp #{:fail} first))
                     first)]
    (second failure)))

(defn extract-states
  "Return each of the execution states seen during the execution of
  the commands for this result list."
  [command-results]
  (->> command-results
       (filter (comp #{:postcondition-check} first))
       (map #(nth % 3))))
