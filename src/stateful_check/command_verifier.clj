(ns stateful-check.command-verifier
  (:require [clojure.walk :as walk]
            [stateful-check.symbolic-values :refer [SymbolicValue] :as symbolic-values]))

(defmulti step-command-runner (fn [state-name & _] state-name))

;; :next-command, :precondition-check, :next-state
;; :pass, :fail

(defmethod step-command-runner :next-command
  [_ command-list results state]
  (if-let [command-list (next command-list)]
    (let [[sym-var [command & args]] (first command-list)]
      [:precondition-check
       (cons [sym-var [command args]]
             (next command-list))
       results
       state])
    [:pass]))

(defmethod step-command-runner :precondition-check
  [_ [[sym-var [command args]] :as command-list] results state]
  (if-let [precondition (:model/precondition command)]
    (try (if (and (every? (fn [arg]
                            (if (satisfies? SymbolicValue arg)
                              (symbolic-values/valid? arg results)
                              true))
                          args) (precondition state args))
           [:next-state
            command-list
            results
            state]
           [:fail])
         (catch Throwable ex
           [:fail ex]))
    [:next-state
     command-list
     results
     state]))

(defmethod step-command-runner :next-state
  [_ [[sym-var [command args]] :as command-list] results state]
  (if-let [next-state (or (:real/next-state command)
                          (:next-state command))]
    (try [:next-command
          command-list
          (conj results sym-var)
          (next-state state args sym-var)]
         (catch Throwable ex
           [:fail ex]))
    [:next-command
     command-list
     results
     state]))

(defmethod step-command-runner :fail [& _])
(defmethod step-command-runner :pass [& _])

(defn valid? [command-list initial-results initial-state]
  (->> [:next-command (cons nil command-list) initial-results initial-state]
       (iterate (partial apply step-command-runner))
       (take-while (complement nil?))
       last
       (= [:pass])))
