(ns stateful-check.command-utils
  (:require [stateful-check.symbolic-values :refer [valid?]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]))

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
  or not the command declares a :model/args function."
  [command state]
  (to-generator (if-let [args (:model/args command)]
                  (args state))))

(defn check-requires
  "Check the requirements for a command to be generated at all, taking
  into account whether or not the command declares a :model/requires
  function."
  [command state]
  (if-let [requires (:model/requires command)]
    (requires state)
    true))

(defn check-precondition
  "Check the precondition for a command, taking into account whether
  or not the command declares a :model/precondition function."
  [command state args]
  (if-let [precondition (:model/precondition command)]
    (precondition state args)
    true))

(defn run-command
  "Run the command's action. If the command has no action then an
  AssertionError will be thrown."
  [command args]
  (if-let [real-command (:real/command command)]
    (apply real-command args)
    (throw (AssertionError. (str "No :real/command function found for " (:name command) " command")))))

(defn model-make-next-state
  "Make the next state for a command, taking into account whether or
  not the command declares a :model/next-state or :next-state
  function."
  [command state args result]
  (if-let [next-state  (or (:model/next-state command)
                           (:next-state command))]
    (next-state state args result)
    state))

(defn real-make-next-state
  "Make the next state for a command, taking into account whether or
  not the command declares a :model/next-state or :next-state
  function."
  [command state args result]
  (if-let [next-state  (or (:real/next-state command)
                           (:next-state command))]
    (next-state state args result)
    state))

(defn check-postcondition
  "Check the postcondition for a command, taking into account whether
  or not the command declares a :real/postcondition function."
  [command prev-state next-state args result]
  (if-let [postcondition (:real/postcondition command)]
    (postcondition prev-state next-state args result)
    true))
