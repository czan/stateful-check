(ns stateful-check.command-verifier
  (:require [stateful-check
             [command-utils :as u]
             [symbolic-values :as symbolic-values :refer [SymbolicValue]]]))

(defmulti ^:private step-command-verifier
  "Step the command verifier state machine one step. Each state in the
  state machine is represented by a \"variant\", which is a vector
  with a key (the state name) and a series of values. What work needs
  to be done in each state is taken care of by this method's
  implementations, and they return a new state variant."
  (fn [state-name & _] state-name))

;; :next-command, :precondition-check, :next-state
;; :pass, :fail

(defmethod step-command-verifier :next-command
  [_ command-list results state]
  (if-let [command-list (next command-list)]
    (let [[sym-var [command & args]] (first command-list)]
      [:precondition-check
       (cons [sym-var [command args]]
             (next command-list))
       results
       state])
    [:pass]))

(defmethod step-command-verifier :precondition-check
  [_ [[sym-var [command args]] :as command-list] results state]
  (try (if (and (every? (fn [arg]
                          (if (satisfies? SymbolicValue arg)
                            (symbolic-values/valid? arg results)
                            true))
                        args)
                (u/check-precondition command state args))
         [:next-state
          command-list
          results
          state]
         [:fail])
       (catch Throwable ex
         [:fail ex])))

(defmethod step-command-verifier :next-state
  [_ [[sym-var [command args]] :as command-list] results state]
  (try [:next-command
        command-list
        (conj results sym-var)
        (u/model-make-next-state command state args sym-var)]
       (catch Throwable ex
         [:fail ex])))

(defmethod step-command-verifier :fail [& _])
(defmethod step-command-verifier :pass [& _])

(defn valid?
  "Validate the given list of commands with the provided initial
  results/state. Returns true if the list of commands is valid, false
  otherwise."
  [command-list initial-results initial-state]
  (->> [:next-command (cons nil command-list) initial-results initial-state]
       (iterate (partial apply step-command-verifier))
       (take-while (complement nil?))
       last
       (= [:pass])))
