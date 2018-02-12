(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r])
  (:import [stateful_check.runner CaughtException]))

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
        init-state-fn (or (:model/initial-state spec)
                          (:initial-state spec)
                          (constantly nil))
        init-state (if (:real/setup spec)
                     (init-state-fn (get bindings g/setup-var))
                     (init-state-fn))]
    (some #(r/valid-execution? % init-state bindings) interleavings)))

(defn spec->property
  "Turn a specification into a testable property."
  ([spec] (spec->property spec nil))
  ([spec options]
   (for-all [commands (g/commands-gen spec (:gen options))]
     (let [runners (r/commands->runners commands)
           setup-fn (:real/setup spec)]
       (dotimes [try (get-in options [:run :max-tries] 1)]
         (let [setup-result (when-let [setup setup-fn]
                              (setup))]
           (try
             (let [bindings (if setup-fn
                              {g/setup-var setup-result}
                              {})
                   results (r/runners->results runners bindings)]
               (when-not (some-valid-interleaving spec commands results bindings)
                 (throw (make-failure-exception (mapv vector
                                                      (:sequential commands)
                                                      (:sequential-strings results))
                                                (mapv (partial mapv vector)
                                                      (:parallel commands)
                                                      (:parallel-strings results))))))
             (finally
               (when-let [cleanup (:real/cleanup spec)]
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
      (.printStackTrace trace))))

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
   (quick-check (get-in options [:run :num-tests] 200)
                (spec->property specification options)
                :seed (get-in options [:run :seed] (System/currentTimeMillis))
                :max-size (get-in options [:gen :max-size] 200))))

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
             '([specification]
               [specification {:gen {:threads 0,
                                     :max-length 10,
                                     :max-size 200}
                               :run {:max-tries 1,
                                     :num-tests 200,
                                     :seed (System/currentTimeMillis)}
                               :report {:first-case? false
                                        :stacktrace? false}}]))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  (let [result-sym (gensym "result")]
    `(let [spec# ~specification
           options# ~options
           results# (run-specification spec# options#)
           ~result-sym (:result results#)
           smallest# (:result (:shrunk results#))]
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
                                      (.printStackTrace ~result-sym (java.io.PrintWriter. *out*))))
                                  (if (failure-exception? smallest#)
                                    (print-execution (failure-exception-data smallest#)
                                                     (get-in options# [:report :stacktrace?] false))
                                    (.printStackTrace smallest# (java.io.PrintWriter. *out*))))
                       :expected (symbol "all executions to match specification"),
                       :actual (symbol "the above execution did not match the specification")}))
       (true? ~result-sym))))
