(ns stateful-check.command-utils
  (:require [clojure.test :refer [report]]
            [clojure.test.check.generators :as gen]))

(defn to-generator
  "Convert a value into a generator, recursively. This means:
    + generator? -> the value
    + sequential? -> gen/tuple with each sub-value already processed
    + map? -> gen/hash-map with each value (not keys) already processed
    + otherwise -> gen/return the value"
  [value]
  (cond (gen/generator? value) value
        (sequential? value) (apply gen/tuple (map to-generator value))
        (map? value) (apply gen/hash-map (mapcat (fn [[k v]]
                                                   [k (to-generator v)])
                                                 value))
        :else (gen/return value)))

(defn args-gen
  "Generate the arguments for a command, taking into account whether
  or not the command declares a :args function."
  [command state]
  (to-generator (when-let [args (:args command)]
                  (args state))))

(defn check-requires
  "Check the requirements for a command to be generated at all, taking
  into account whether or not the command declares a :requires
  function."
  [command state]
  (if-let [requires (:requires command)]
    (requires state)
    true))

(defn check-precondition
  "Check the precondition for a command, taking into account whether
  or not the command declares a :precondition function."
  [command state args]
  (if-let [precondition (:precondition command)]
    (precondition state args)
    true))

(defn make-next-state
  "Make the next state for a command, taking into account whether or
  not the command declares a :next-state function."
  [command state args result]
  (if-let [next-state  (:next-state command)]
    (next-state state args result)
    state))

(defn check-postcondition
  "Check the postcondition for a command, taking into account whether or
  not the command declares a :postcondition function. Returns nil if
  the postcondition passes, otherwise returns a map with a :message
  key describing the failure. If the postcondition made any
  clojure.test assertions, events of type :fail and :error are added
  to the result under the :events key."
  [command prev-state next-state args result]
  (if-let [postcondition (:postcondition command)]
    (let [events (atom [])]
      (binding [report (fn [event] (swap! events conj event))]
        (let [postcondition-result (postcondition prev-state next-state args result)
              all-events           @events
              pass-events          (filter (comp #{:pass} :type) all-events)
              failure-events       (filter (comp #{:fail :error} :type) all-events)]
          (cond
            ;; If we have explicit failure events, fail the
            ;; postcondition.
            (seq failure-events) {:message "Postcondition reported failures."
                                  :events failure-events}
            ;; If we have explicit pass events, and no failure events,
            ;; then pass the test.
            (seq pass-events)    nil
            ;; If we don't have pass or fail events, then just use the
            ;; postcondition result by itself.
            postcondition-result nil
            :else                {:message "Postcondition returned falsey."}))))
    nil))
