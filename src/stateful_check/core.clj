(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r]
            [stateful-check.core-utils :as utils]))

(defn ^{:deprecated "0.3.0"} reality-matches-model
  "Create a property which checks a given stateful-check
  specification."
  [spec]
  (utils/spec->property spec))

(defn print-test-results
  {:deprecated "0.3.0",
   :doc (:doc (meta #'utils/print-test-results))
   :arglists (:arglists (meta #'utils/print-test-results))}
  [spec results options]
  (utils/print-test-results spec results options))

(defn make-failure-exception [sequential-trace parallel-trace]
  (ex-info "Generative test failed."
           {:sequential sequential-trace
            :parallel parallel-trace}))

(def counter (atom 0))

(defn spec->property
  "Turn a specification into a testable property."
  ([spec] (spec->property spec {:max-tries 1}))
  ([spec {:keys [max-tries] :as options}]
   (reset! counter 0)
   (for-all [commands (g/commands-gen spec options)]
     (dotimes [try (or max-tries 1)]
       (let [setup-result (when-let [setup (:real/setup spec)]
                            (setup))
             init-state-fn (or (:model/initial-state spec)
                               (:initial-state spec)
                               (constantly nil))
             init-state (if (:real/setup spec)
                          (init-state-fn setup-result)
                          (init-state-fn))
             bindings (if (:real/setup spec)
                        {g/setup-var setup-result}
                        {})]
         (try
           (let [results (r/run-commands commands init-state bindings)
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
               (if (:real/setup spec)
                 (cleanup setup-result)
                 (cleanup)))))))
     true)))

(defn print-execution-exception
  ([{:keys [sequential parallel]}]
   (print-execution-exception sequential parallel))
  ([sequential parallel]
   (printf "Sequential prefix:\n")
   (doseq [[[handle cmd & args] trace] sequential]
     (printf "  %s = %s = %s\n"
             (pr-str handle)
             (cons (:name cmd)
                   args)
             trace))
   (doseq [[i thread] (map vector (range) parallel)]
     (printf "\nThread %s:\n" (inc i))
     (doseq [[[handle cmd & args] trace] thread]
       (printf "  %s = %s = %s\n"
               (pr-str handle)
               (cons (:name cmd)
                     args)
               trace)))))

(def test-spec
  (let [map-keys ["" "a" "house" "tree" "λ"]]
    {:commands {:new {:real/command (fn [] (java.util.TreeMap.))
                      :next-state (fn [states _ result]
                                    (conj states [result nil]))}
                :put {:model/requires (fn [states] (seq states))
                      :model/args (fn [states]
                                    [(gen/elements (map first states))
                                     (gen/elements map-keys)
                                     gen/int])
                      :real/command (fn [^java.util.Map map key val]
                                      (.put map key val))
                      :next-state (fn [states [m k v] _]
                                    (map (fn [[map contents]]
                                           (if (identical? m map)
                                             [map (assoc contents k v)]
                                             [map contents]))
                                         states))}
                :get {:model/requires (fn [states] (and (seq states)
                                                       (seq (keys (apply merge (map second states))))))
                      :model/args (fn [states]
                                    [(gen/elements (map first states))
                                     (gen/elements map-keys)])
                      :real/command (fn [^java.util.Map map key]
                                      (.get map key))
                      :real/postcondition (fn [prev-state _ [m k] val]
                                            (= (get (some (fn [[map contents]]
                                                            (when (identical? m map)
                                                              contents))
                                                          prev-state)
                                                    k)
                                               val))}}}))

;; (def result (quick-check 200 (spec->property test-spec {:max-tries 1, :parallel-factor 2})))

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
             (:arglists (meta #'run-specification)))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  `(let [spec# ~specification
         options# ~options
         results# (run-specification spec# options#)
         result# (:result results#)
         smallest# (:result (:shrunk results#))]
     (println results#)
     (if (true? (:result results#))
       (t/do-report {:type :pass,
                     :message ~msg,
                     :expected true,
                     :actual true})
       (t/do-report {:type :fail,
                     :message (with-out-str
                                (when-let [msg# ~msg]
                                  (println msg#))
                                (when (:print-first-case? options#)
                                  (print-execution-exception (ex-data result#)))
                                (print-execution-exception (ex-data smallest#)))
                     :expected (symbol "all executions to match specification"),
                     :actual (symbol "the above execution did not match the specification")}))
     (true? smallest#)))
