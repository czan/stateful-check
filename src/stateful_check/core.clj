(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r])
  (:import [stateful_check.runner CaughtException]))

(def default-num-tests 200)
(def default-max-tries 1)
(def default-timeout-ms 0)

(defn- make-failure-exception [sequential-trace parallel-trace]
  (ex-info "Generative test failed."
           {:sequential sequential-trace
            :parallel parallel-trace}))

(defn failure-exception? [ex]
  (and (instance? clojure.lang.ExceptionInfo ex)
       (= (.getMessage ^clojure.lang.ExceptionInfo ex)
          "Generative test failed.")))

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
    [command
     (cond
       (= ::r/unevaluated result) result
       (instance? CaughtException result) result
       (= last-str result-str) result-str
       :else (str result-str
                  "\n    >> object may have been mutated later into " last-str " <<\n"))]))

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
                   results (r/runners->results runners bindings (get-in options [:run :timeout-ms] default-timeout-ms))]
               (when-not (some-valid-interleaving spec commands results bindings)
                 (throw (make-failure-exception (mapv combine-cmds-with-traces
                                                      (:sequential commands)
                                                      (:sequential results)
                                                      (:sequential-strings results))
                                                (mapv (partial mapv combine-cmds-with-traces)
                                                      (:parallel commands)
                                                      (:parallel results)
                                                      (:parallel-strings results))))))
             (catch clojure.lang.ExceptionInfo ex
               (if (= (.getMessage ex) "Timed out")
                 (let [results (ex-data ex)]
                   (throw (make-failure-exception (mapv combine-cmds-with-traces
                                                        (:sequential commands)
                                                        (:sequential results)
                                                        (:sequential-strings results))
                                                  (mapv (partial mapv combine-cmds-with-traces)
                                                        (:parallel commands)
                                                        (:parallel results)
                                                        (:parallel-strings results)))))
                 (throw ex)))
             (finally
               (when-let [cleanup (:cleanup spec)]
                 (if setup-fn
                   (cleanup setup-result)
                   (cleanup))))))))
     true)))

(defn- print-sequence [commands stacktrace?]
  (doseq [[[handle cmd & args] trace] commands]
    (printf "  %s = %s %s\n"
            (pr-str handle)
            (cons (:name cmd)
                  args)
            (if (= ::r/unevaluated trace)
              ""
              (str " = "
                   (if (instance? CaughtException trace)
                     (if stacktrace?
                       (with-out-str
                         (.printStackTrace ^Throwable (:exception trace)
                                           (java.io.PrintWriter. *out*)))
                       (.toString ^Object (:exception trace)))
                     trace))))))

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
  generates test cases and runs them. If run with in an `is`, it will
  report details (and pretty-print them) if it fails.

  The `options` map consists of three potential keys: `:gen`, `:run`,
  and `:report`, each of which influence a different part of the test.

  `:gen` has four sub-keys:
   - `:threads` specifies how many parallel threads to execute
   - `:max-length` specifies a max length for command sequences
   - `:max-size` specifies a maximum size for generated values
   - `:shrink-strategies` specifies a sequence of shrink strategies
     that should be tried (in order) to reduce the size of a failing
     test (see `stateful-check.generator/default-shrink-strategies`
     and `stateful-check.shrink-strategies`)

  `:run` has three sub-keys:
   - `:max-tries` specifies how attempts to make to fail a test
   - `:num-tests` specifies how many tests to run
   - `:seed` specifies the initial seed to use for generation
   - `:timeout-ms` specifies the maximum number of milliseconds that a
     test is permitted to run for - taking longer is considered a
     failure (default: 0, meaning no timeout; see NOTE below for more
     details)

  `:report` has two sub-keys, but only works within an `is`:
   - `:first-case?` specifies whether to print the first failure
   - `:stacktrace?` specifies whether to print exception stacktraces

  The `:timeout-ms` option is unsafe in general, but may be helpful in
  some circumstances. It allows you to categorise a test as a failure
  if it takes more than a given time, but each of the threads must
  respond to being interrupted by completing and shutting down. If
  these threads do not shut themselves down then they may continue to
  consume system resources (CPU and memory, among other things),
  impacting other tests."
  ([specification] (specification-correct? specification nil))
  ([specification options]
   (true? (:result (run-specification specification options)))))
;; We need this to be a separate form, for some reason. The attr-map
;; in defn doesn't work if you use the multi-arity form.
(alter-meta! #'specification-correct? assoc :arglists
             `([~'specification]
               [~'specification {:gen {:threads ~g/default-threads
                                       :max-length ~g/default-max-length
                                       :max-size ~g/default-max-size
                                       :shrink-strategies g/default-shrink-strategies}
                                 :run {:max-tries ~default-max-tries
                                       :num-tests ~default-num-tests
                                       :seed (System/currentTimeMillis)
                                       :timeout-ms ~default-timeout-ms}
                                 :report {:first-case? false
                                          :stacktrace? false}}]))

(defn report-result [msg _ options results]
  (let [result (:result results)
        smallest (get-in results [:shrunk :result])]
    (if (true? result)
      (t/do-report {:type :pass,
                    :message msg,
                    :expected true,
                    :actual true})
      (t/do-report {:type :fail,
                    :message (with-out-str
                               (binding [*out* (java.io.PrintWriter. *out*)]
                                 (when msg
                                   (println msg))
                                 (when (get-in options [:report :first-case?] false)
                                   (println "  First failing test case")
                                   (println "  -----------------------------")
                                   (if (failure-exception? result)
                                     (print-execution (failure-exception-data result)
                                                      (get-in options [:report :stacktrace?] false))
                                     (.printStackTrace ^Throwable result
                                                       ^java.io.PrintWriter *out*))
                                   (println)
                                   (println "  Smallest case after shrinking")
                                   (println "  -----------------------------"))
                                 (if (failure-exception? smallest)
                                   (print-execution (failure-exception-data smallest)
                                                    (get-in options [:report :stacktrace?] false))
                                   (.printStackTrace ^Throwable smallest
                                                     ^java.io.PrintWriter *out*))
                                 (println)
                                 (println "Seed:" (:seed results))
                                 (when (> (get-in options [:gen :threads] 0) 1)
                                   (println (str "  Note: Test cases with multiple threads are not deterministic, so using the\n"
                                                 "        same seed does not guarantee the same result.")))))
                    :expected (symbol "all executions to match specification"),
                    :actual (symbol "the above execution did not match the specification")}))
    (true? result)))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  `(let [spec# ~specification
         options# ~options
         results# (run-specification spec# options#)]
     (report-result ~msg spec# options# results#)))
