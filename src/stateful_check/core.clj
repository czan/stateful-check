(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r])
  (:import [stateful_check.runner CaughtException]))

(def default-num-tests 200)
(def default-max-tries 1)

(defn- make-failure-exception [sequential-trace parallel-trace]
  (ex-info "Generative test failed."
           {:sequential sequential-trace
            :parallel parallel-trace}))

(defn failure-exception? [ex]
  (and (instance? clojure.lang.ExceptionInfo ex)
       (= (.getMessage ex) "Generative test failed.")))

(defn failure-exception-data [ex]
  (ex-data ex))

(defn- some-valid-interleaving [spec commands results bindings]
  (let [interleavings (g/every-interleaving (mapv vector
                                                  (:sequential commands)
                                                  (:sequential results))
                                            (mapv (partial mapv vector)
                                                  (:parallel commands)
                                                  (:parallel results)))
        init-state-fn (or (:initial-state spec)
                          (constantly nil))
        init-state (if (:setup spec)
                     (init-state-fn (get bindings g/setup-var))
                     (init-state-fn))]
    (some #(r/valid-execution? % init-state bindings) interleavings)))

(defn combine-cmds-with-traces [command result result-str]
  (let [last-str (pr-str result)]
    (if (= last-str result-str)
      [command result-str]
      [command (str result-str
                    "\n    >> object may have been mutated later into " last-str " <<\n")])))

(defn spec->property
  "Turn a specification into a testable property."
  ([spec] (spec->property spec nil))
  ([spec options]
   (for-all [commands (g/commands-gen spec (:gen options))]
     (let [runners (r/commands->runners commands)
           setup-fn (:setup spec)]
       (dotimes [try (get-in options [:run :max-tries] default-max-tries)]
         (let [setup-result (when-let [setup setup-fn]
                              (setup))]
           (try
             (let [bindings (if setup-fn
                              {g/setup-var setup-result}
                              {})
                   results (r/runners->results runners bindings)]
               (when-not (some-valid-interleaving spec commands results bindings)
                 (throw (make-failure-exception (mapv combine-cmds-with-traces
                                                      (:sequential commands)
                                                      (:sequential results)
                                                      (:sequential-strings results))
                                                (mapv (partial mapv combine-cmds-with-traces)
                                                      (:parallel commands)
                                                      (:parallel results)
                                                      (:parallel-strings results))))))
             (finally
               (when-let [cleanup (:cleanup spec)]
                 (if setup-fn
                   (cleanup setup-result)
                   (cleanup))))))))
     true)))

(defn- print-sequence [commands stacktrace?]
  (doseq [[[handle cmd & args] trace] commands]
    (printf "  %s = %s = %s\n"
            (pr-str handle)
            (cons (:name cmd)
                  args)
            (if (instance? CaughtException trace)
              (.toString (:exception trace))
              trace))
    (if (and (instance? CaughtException trace)
             stacktrace?)
      (.printStackTrace ^Throwable trace))))

(defn print-execution
  ([{:keys [sequential parallel]} stacktrace?]
   (print-execution sequential parallel stacktrace?))
  ([sequential parallel stacktrace?]
   (printf "Sequential prefix:\n")
   (print-sequence sequential stacktrace?)
   (doseq [[i thread] (map vector (range) parallel)]
     (printf "\nThread %s:\n" (g/index->letter i))
     (print-sequence thread stacktrace?))))

(defn run-specification
  "Run a specification. This will convert the spec into a property and
  run it using clojure.test.check/quick-check. This function then
  returns the full quick-check result."
  ([specification] (run-specification specification nil))
  ([specification options]
   (quick-check (get-in options [:run :num-tests] default-num-tests)
                (spec->property specification options)
                :seed (get-in options [:run :seed] (System/currentTimeMillis))
                :max-size (get-in options [:gen :max-size] g/default-max-size))))

(defn specification-correct?
  "Test whether or not the specification matches reality. This
  generates test cases and runs them.

  When used within an `is` expression two extra options can be
  supplied under the :report key:

    :first-case? instructs the test-case printer to print the command
  list prior to shrinking, as well as the smallest case found

    :print-stacktrace? instructs the test-case printer to print the
  stacktrace of any exceptions thrown"
  ([specification] (specification-correct? specification nil))
  ([specification options]
   (true? (:result (run-specification specification options)))))
;; We need this to be a separate form, for some reason. The attr-map
;; in defn doesn't work if you use the multi-arity form.
(alter-meta! #'specification-correct? assoc :arglists
             `([~'specification]
               [~'specification {:gen {:threads ~g/default-threads
                                       :max-length ~g/default-max-length
                                       :max-size ~g/default-max-size}
                                 :run {:max-tries ~default-max-tries
                                       :num-tests ~default-num-tests
                                       :seed (System/currentTimeMillis)}
                                 :report {:first-case? false
                                          :stacktrace? false}}]))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  (let [result-sym (gensym "result")
        smallest-sym (gensym "smallest")]
    `(let [spec# ~specification
           options# ~options
           results# (run-specification spec# options#)
           ~result-sym (:result results#)
           ~smallest-sym (:result (:shrunk results#))]
       (if (true? ~result-sym)
         (t/do-report {:type :pass,
                       :message ~msg,
                       :expected true,
                       :actual true})
         (t/do-report {:type :fail,
                       :message (with-out-str
                                  ~(when msg
                                     `(println ~msg))
                                  (when (get-in options# [:report :first-case?] false)
                                    (if (failure-exception? ~result-sym)
                                      (print-execution (failure-exception-data ~result-sym)
                                                       (get-in options# [:report :stacktrace?] false))
                                      (.printStackTrace ~(vary-meta result-sym assoc :tag `Throwable)
                                                        (java.io.PrintWriter. *out*))))
                                  (if (failure-exception? ~smallest-sym)
                                    (print-execution (failure-exception-data ~smallest-sym)
                                                     (get-in options# [:report :stacktrace?] false))
                                    (.printStackTrace ~(vary-meta smallest-sym assoc :tag `Throwable)
                                                      (java.io.PrintWriter. *out*))))
                       :expected (symbol "all executions to match specification"),
                       :actual (symbol "the above execution did not match the specification")}))
       (true? ~result-sym))))
