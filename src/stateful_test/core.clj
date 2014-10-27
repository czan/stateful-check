(ns stateful-test.core
  (:require [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.properties :refer [for-all]]
            [stateful-test.gen :refer [gdo gen-do]]))

(defrecord Var [num])
(defmethod print-method Var
  [v ^java.io.Writer writer]
  (.write writer (str "#<" (:num v) ">")))

(defn generate-commands* [spec count state]
  "Returns a list of rose-trees *within* the monad of gen/gen-pure"
  (gen/sized
   (fn [size]
     (gen/frequency
      [[1 (gen/gen-pure nil)]
       [size (gen-do com-rose <- ((:generate-command spec) state)
                     :let [com (rose/root com-rose)
                           command (get (:commands spec) com)
                           _ (assert command (str "Command " com " not found in :commands map"))]
                     rose <- (if-let [f (:model/args command)]
                               (f state)
                               (gen/return []))
                     :let [args (rose/root rose)
                           precondition-passed? (if-let [f (:model/precondition command)]
                                                  (f state args)
                                                  true)]
                     (if precondition-passed?
                       (gen-do :let [result (->Var count)
                                     next-state (if-let [f (or (:model/next-state command)
                                                               (:next-state command))]
                                                  (f state args result)
                                                  state)]
                               roses <- (gen/resize (dec size) (generate-commands* spec (inc count) next-state))
                               (gen/gen-pure (cons (rose/fmap (fn [args]
                                                                [result (cons com args)])
                                                              rose)
                                                   roses)))
                       (generate-commands* spec count state)))]]))))

(defn generate-commands [spec state]
  (gen/gen-bind (generate-commands* spec 0 state)
                (fn [roses]
                  (gen/gen-pure (rose/shrink vector roses)))))

(defn run-commands [spec generated-commands]
  (try
    (reduce (fn [[state results] [result-var [com & args]]]
              (let [command (get (:commands spec) com)
                    args (replace results args)
                    result (do (assert (:real/command command) (str "Command " com " does not have a :real/command function"))
                               (apply (:real/command command) args))
                    passed-postcondition? (if-let [f (:real/postcondition command)]
                                            (f state args result)
                                            true)]
                (if passed-postcondition?
                  [(if-let [f (or (:real/next-state command)
                                   (:next-state command))]
                     (f state args result)
                     state)
                   (assoc results result-var result)]
                  (throw (Exception. (str "Postcondition failed running " (cons com args)))))))
            nil generated-commands)
    (finally
      (if-let [f (:cleanup spec)]
        (f))))
  true)

(defn valid-commands? [spec command-list]
  (first (reduce (fn [[valid? state results] [result-var [com & args]]]
                   (let [command (get (:commands spec) com)
                         precondition-passed? (and
                                               (every? (fn [arg]
                                                         (or (not (instance? Var arg))
                                                             (contains? results arg)))
                                                       args)
                                               (if-let [f (:model/precondition command)]
                                                     (f state args)
                                                     true))]
                     (if precondition-passed?
                       (if-let [f (or (:model/next-state command)
                                      (:next-state command))]
                         [true (f state args result-var) (conj results result-var)]
                         [true state results])
                       (reduced [false nil nil]))))
                 [true nil #{}] command-list)))

(defn reality-matches-model? [spec]
  (for-all [commands (gen/such-that (partial valid-commands? spec)
                                    (generate-commands spec nil))]
    (run-commands spec commands)))



(defn print-command-output [spec generated-commands with-states?]
  (try (reduce (fn [[state results] [result-var [com & raw-args]]]
                 (let [command (get (:commands spec) com)
                       args (replace results raw-args)
                       result (do (assert (:real/command command) (str "Command " com " does not have a :real/command function"))
                                  (apply (:real/command command) args))
                       passed-postcondition? (if-let [f (:real/postcondition command)]
                                               (f state args result)
                                               true)]
                   (println "  " result-var "=" (cons com raw-args) "\t;=>" result (if with-states? state ""))
                   (if passed-postcondition?
                     [(if-let [f (or (:real/next-state command)
                                     (:next-state command))]
                        (f state args result)
                        state)
                      (assoc results result-var result)]
                     (reduced nil))))
               nil generated-commands)
       (finally
         (if-let [f (:cleanup spec)]
           (f)))))

(defn print-test-results
  ([spec results] (print-test-results spec results false))
  ([spec results with-states?]
     (when-not (true? (:result results))
       (println "\nFailing test case:")
       (print-command-output spec (-> results :fail first) with-states?)
       (println "Shrunk:")
       (print-command-output spec (-> results :shrunk :smallest first) with-states?))))



