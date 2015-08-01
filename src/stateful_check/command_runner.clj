(ns stateful-check.command-runner
  (:require [clojure.walk :as walk]
            [stateful-check
             [command-utils :as u]
             [symbolic-values :refer [get-real-value SymbolicValue ->RootVar]]]))

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
  [_ spec command-list results state]
  (if (seq command-list)
    (try (if (u/check-spec-postcondition spec state)
           (let [[sym-var [command & raw-args]] (first command-list)
                 args (walk/prewalk (fn [value]
                                      (if (satisfies? SymbolicValue value)
                                        (get-real-value value results)
                                        value))
                                    raw-args)]
             [:run-command spec
              [sym-var [command args raw-args]]
              (next command-list)
              results
              state])
           [:fail spec state])
         (catch Throwable ex
           [:fail spec state ex]))
    [:pass spec]))

(defmethod step-command-runner :run-command
  [_ spec [sym-var [command args raw-args] :as current] command-list results state]
  (try (let [result (u/run-command command args)
             results (assoc results sym-var result)]
         [:next-state spec
          current
          command-list
          results
          state
          result])
       (catch Throwable ex
         [:fail spec state ex])))

(defmethod step-command-runner :next-state
  [_ spec [sym-var [command args raw-args] :as current] command-list results state result]
  (try [:postcondition-check spec
        current
        command-list
        results
        (u/real-make-next-state command state args result)
        state
        result]
       (catch Throwable ex
         [:fail spec state ex])))

(defmethod step-command-runner :postcondition-check
  [_ spec [sym-var [command args raw-args] :as current] command-list results next-state prev-state result]
  (try (if (u/check-postcondition command prev-state next-state args result)
         [:next-command spec
          command-list
          results
          next-state]
         [:fail spec next-state])
       (catch Throwable ex
         [:fail spec next-state ex])))

;; terminal states, so return `nil`
(defmethod step-command-runner :fail [spec state & _]
  (u/run-spec-cleanup spec state)
  nil)
(defmethod step-command-runner :pass [spec state & _]
  (u/run-spec-cleanup spec state)
  nil)

(defn run-commands
  "Run the given list of commands with the provided initial
  results/state. Returns a lazy seq of states from the command
  runner."
  [spec command-list]
  (let [state-fn (or (:real/initial-state spec)
                     (:initial-state spec)
                     (constantly nil))
        setup-fn (:real/setup spec)
        setup-value (if setup-fn (setup-fn))
        results (if setup-fn
                  {(->RootVar "setup") setup-value})
        state (if setup-fn
                (state-fn setup-value)
                (state-fn))]
    (->> [:next-command spec command-list results state]
         (iterate (partial apply step-command-runner))
         (take-while (complement nil?)))))

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
