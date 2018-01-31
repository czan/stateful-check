(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r]))

(defn make-failure-exception [sequential-trace parallel-trace]
  (ex-info "Generative test failed."
           {:sequential sequential-trace
            :parallel parallel-trace}))

(defn spec->property
  "Turn a specification into a testable property."
  ([spec] (spec->property spec {:max-tries 1}))
  ([spec {:keys [max-tries] :as options}]
   (for-all [commands (g/commands-gen spec options)]
     (let [runners (r/commands->runners commands)]
       (dotimes [try (or max-tries 1)]
         (let [setup-fn (:real/setup spec)
               setup-result (when-let [setup setup-fn]
                              (setup))]
           (try
             (let [init-state-fn (or (:model/initial-state spec)
                                     (:initial-state spec)
                                     (constantly nil))
                   init-state (if setup-fn
                                (init-state-fn setup-result)
                                (init-state-fn))
                   bindings (if setup-fn
                              {g/setup-var setup-result}
                              {})
                   results (r/runners->results runners bindings)
                   interleavings (g/every-interleaving (mapv vector
                                                             (:sequential commands)
                                                             (:sequential results))
                                                       (mapv (partial mapv vector)
                                                             (:parallel commands)
                                                             (:parallel results)))]
               (when-not (some #(r/valid-execution? % init-state bindings) interleavings)
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

(defn print-execution
  ([{:keys [sequential parallel]}]
   (print-execution sequential parallel))
  ([sequential parallel]
   (printf "Sequential prefix:\n")
   (doseq [[[handle cmd & args] trace] sequential]
     (printf "  %s = %s = %s\n"
             (pr-str handle)
             (cons (:name cmd)
                   args)
             trace))
   (doseq [[i thread] (map vector (range) parallel)]
     (printf "\nThread %s:\n" (g/index->letter i))
     (doseq [[[handle cmd & args] trace] thread]
       (printf "  %s = %s = %s\n"
               (pr-str handle)
               (cons (:name cmd)
                     args)
               trace)))))

(defn run-specification
  "Run a specification. This will convert the spec into a property and
  run it using clojure.test.check/quick-check. This function then
  returns the full quick-check result."
  ([specification] (run-specification specification nil))
  ([specification {:keys [num-tests max-size seed] :as options}]
   (quick-check (or num-tests 100)
                (spec->property specification options)
                :seed seed
                :max-size (or max-size 200))))

(defn specification-correct?
  "Test whether or not the specification matches reality. This
  generates test cases and runs them.

  When used within an `is` expression two extra options can be
  supplied:

    :print-first-case?, which instructs the test-case printer to print
  the command list prior to shrinking as well as the shrunk list

    :print-stacktrace?, which instructs the test-case printer to print
  the stacktrace of any exceptions"
  ([specification] (specification-correct? specification nil))
  ([specification options]
   (true? (:result (run-specification specification options)))))
;; We need this to be a separate form, for some reason. The attr-map
;; in defn doesn't work if you use the multi-arity form.
(alter-meta! #'specification-correct? assoc :arglists
             '([specification]
               [specification {:num-tests 100
                               :max-size 200
                               :seed (default-seed)
                               :max-tries 1}])
             ;; (:arglists (meta #'run-specification))
             )

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  `(let [spec# ~specification
         options# ~options
         results# (run-specification spec# options#)
         result# (:result results#)
         smallest# (:result (:shrunk results#))]
     (if (true? result#)
       (t/do-report {:type :pass,
                     :message ~msg,
                     :expected true,
                     :actual true})
       (t/do-report {:type :fail,
                     :message (with-out-str
                                (println ~msg)
                                (when (:print-first-case? options#)
                                  (print-execution (ex-data result#)))
                                (print-execution (ex-data smallest#)))
                     :expected (symbol "all executions to match specification"),
                     :actual (symbol "the above execution did not match the specification")}))
     (true? result#)))
