(ns stateful-check.command-utils
  (:require [clojure.test.check.generators :as gen]))

(defn generate-args
  "Generate the arguments for a command, taking into account whether
  or not the command declares a :model/args function."
  [command state]
  (if-let [args (:model/args command)]
    (args state)
    (gen/return [])))

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

(defn check-spec-postcondition
  "Check the postcondition for the specification, taking into account
  whether or not the specification declares a :real/postcondition
  function."
  [spec state]
  (if-let [postcondition (:real/postcondition spec)]
    (postcondition state)
    true))
