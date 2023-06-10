(ns stateful-check.core
  (:require [clojure.pprint :refer [print-table]]
            [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all] :as p]
            [clojure.test.check.results :refer [Result pass? result-data]]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r])
  (:import [stateful_check.runner CaughtException]))

(def default-num-tests 200)
(def default-max-tries 1)
(def default-timeout-ms 0)
(def default-assume-immutable-results false)

(defrecord TestResult [pass? result-data]
  Result
  (pass? [this] pass?)
  (result-data [this] result-data))

(defn- failure-messages
  "Return a map mapping from a command handle to a set of messages
  indicating failures that occurred during all the interleavings of a
  command set."
  [spec commands results bindings]
  (let [interleavings (g/every-interleaving (mapv vector
                                                  (:sequential commands)
                                                  (:sequential results))
                                            (mapv (partial mapv vector)
                                                  (:parallel commands)
                                                  (:parallel results)))
        init-state-fn (or (:initial-state spec)
                          (constantly nil))
        init-state    (if (:setup spec)
                        (init-state-fn (get bindings g/setup-var))
                        (init-state-fn))
        messages      (map #(r/failure-message % init-state bindings) interleavings)]
    (when (every? some? messages) ;; if all paths failed
      (->> messages
           (map (fn [[handle message]]
                    {handle #{message}}))
           (apply merge-with into)))))

(defn combine-cmds-with-traces [command result result-str]
  [command
   (cond
     (= ::r/unevaluated result) result
     (instance? CaughtException result) result
     :else (if (nil? result-str)
             (pr-str result)
             (let [last-str (pr-str result)]
               (if (= result-str last-str)
                 result-str
                 (str result-str
                      "\n    >> object may have been mutated later into " last-str " <<\n")))))])

(def ^:dynamic *run-commands* nil)

(defn build-test-runner [specification commands timeout-ms assume-immutable-results]
  "Return a function to execute each of `commands` and report a `TestResult`."
  (let [runners (r/commands->runners commands)]
    (fn []
      (let [setup-result (when-let [setup (:setup specification)]
                           (setup))]
        (try
          (let [bindings (if (:setup specification)
                           {g/setup-var setup-result}
                           {})
                results (r/runners->results runners bindings timeout-ms assume-immutable-results)]
            (if-let [messages (failure-messages specification commands results bindings)]
              (->TestResult false
                            {:message "Test failed."
                             :messages messages
                             :sequential (mapv combine-cmds-with-traces
                                               (:sequential commands)
                                               (:sequential results)
                                               (:sequential-strings results))
                             :parallel (mapv (partial mapv combine-cmds-with-traces)
                                             (:parallel commands)
                                             (:parallel results)
                                             (:parallel-strings results))})
              (->TestResult true {})))
          (catch clojure.lang.ExceptionInfo ex
            (->TestResult
             false
             (if (= (.getMessage ex) "Timed out")
               (let [results (ex-data ex)]
                 {:message "Test timed out."
                  :messages {nil (format "Test timed out after %sms" timeout-ms)}
                  :sequential (mapv combine-cmds-with-traces
                                    (:sequential commands)
                                    (:sequential results)
                                    (:sequential-strings results))
                  :parallel (mapv (partial mapv combine-cmds-with-traces)
                                  (:parallel commands)
                                  (:parallel results)
                                  (:parallel-strings results))})
               ;; Any other type of exception is re-thrown, because it's
               ;; unexpected. The command runners catch exceptions where they
               ;; are expected, so any other exceptions represent programmer
               ;; error and should fail the test immediately.
               (throw ex))))
          (finally
            (when-let [cleanup (:cleanup specification)]
              (if (:setup specification)
                (cleanup setup-result)
                (cleanup)))))))))

(defn spec->property
  "Turn a specification into a testable property."
  ([spec] (spec->property spec nil))
  ([spec options]
   (for-all [commands (g/commands-gen spec (:gen options))]
     (when *run-commands*
       (doseq [cmds (cons (:sequential commands)
                          (:parallel commands))]
         (->> cmds
              (into {} (map (fn [[_ {:keys [name]} _]]
                              [name 1])))
              (swap! *run-commands* #(merge-with + %1 %2)))))
     (let [run-test (build-test-runner spec
                                       commands
                                       (get-in options [:run :timeout-ms] default-timeout-ms)
                                       (get-in options [:run :assume-immutable-results] default-assume-immutable-results))]
       (loop [tries-left (get-in options [:run :max-tries] default-max-tries)]
         (if (zero? tries-left)
           (->TestResult true {:commands commands, :options options, :specification spec})
           (let [try-result (run-test)]
             (if (pass? try-result)
               (recur (dec tries-left))
               (update try-result :result-data
                       merge {:commands commands, :options options, :specification spec})))))))))

(defn- print-sequence [commands stacktrace? messages]
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
                     trace))))
    (doseq [message (get messages handle)
            line    (.split ^String message "\n")]
      (printf "    %s\n" line))))

(defn print-execution [{:keys [message sequential parallel messages]} stacktrace?]
  (printf "Sequential prefix:\n")
  (print-sequence sequential stacktrace? messages)
  (doseq [[i thread] (map vector (range) parallel)]
    (printf "\nThread %s:\n" (g/index->letter i))
    (print-sequence thread stacktrace? messages))
  (doseq [message (get messages nil)]
    (println message)))

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
   - `:assume-immutable-results` specifies whether the runner should
     assume that the results of running commands are immutable, and
     thus delay string converstions until the end of the test run
     (default: false)

  `:report` has two sub-keys, but only works within an `is`:
   - `:first-case?` specifies whether to print the first failure
   - `:stacktrace?` specifies whether to print exception stacktraces
   - `:command-frequency?` specifies whether to print information
     about how often each command was run

  The `:timeout-ms` option is unsafe in general, but may be helpful in
  some circumstances. It allows you to categorise a test as a failure
  if it takes more than a given time, but each of the threads must
  respond to being interrupted by completing and shutting down. If
  these threads do not shut themselves down then they may continue to
  consume system resources (CPU and memory, among other things),
  impacting other tests."
  ([specification] (specification-correct? specification nil))
  ([specification options]
   (:pass? (run-specification specification options))))
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
                                          :stacktrace? false
                                          :command-frequency? false}}]))

(defn report-result [msg _ options results frequencies]
  (let [result-data (:result-data results)
        smallest-result-data (get-in results [:shrunk :result-data])]
    (when (get-in options [:report :command-frequency?] false)
      (print "Command execution counts:")
      (print-table (->> frequencies
                        (sort-by val)
                        reverse ;; big numbers on top
                        (map #(hash-map :command (key %)
                                        :count (val %))))))
    (cond
      (::p/error result-data)
      (t/do-report {:type :error,
                    :fault :true
                    :message msg
                    :expected nil,
                    :actual (::p/error result-data)})

      (:pass? results)
      (t/do-report {:type :pass,
                    :message msg,
                    :expected true,
                    :actual true})

      :else
      (t/do-report {:type :fail,
                    :message (with-out-str
                               (binding [*out* (java.io.PrintWriter. *out*)]
                                 (when msg
                                   (println msg))
                                 (when (get-in options [:report :first-case?] false)
                                   (println "  First failing test case")
                                   (println "  -----------------------------")
                                   (print-execution result-data
                                                    (get-in options [:report :stacktrace?] false))
                                   (println)
                                   (println "  Smallest case after shrinking")
                                   (println "  -----------------------------"))
                                 (if (::p/error smallest-result-data)
                                   (.printStackTrace ^Throwable (::p/error smallest-result-data)
                                                     ^java.io.PrintWriter *out*)
                                   (print-execution smallest-result-data
                                                    (get-in options [:report :stacktrace?] false)))
                                 (println)
                                 (println "Seed:" (:seed results))
                                 (when (> (get-in options [:gen :threads] 0) 1)
                                   (println (str "  Note: Test cases with multiple threads are not deterministic, so using the\n"
                                                 "        same seed does not guarantee the same result.")))))
                    :expected (symbol "all executions to match specification"),
                    :actual (symbol "the above execution did not match the specification")}))
    (:pass? results)))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  `(let [spec# ~specification
         options# ~options
         [results# frequencies#] (binding [*run-commands* (atom {})]
                                   [(run-specification spec# options#)
                                    @*run-commands*])]
     (report-result ~msg spec# options# results# frequencies#)))
