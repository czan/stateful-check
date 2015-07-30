(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check
             [generators :as gen]
             [properties :refer [for-all]]
             [rose-tree :as rose]]
            [stateful-check
             [command-runner :as r]
             [command-utils :as u]
             [command-verifier :as v]
             [gen :refer [gen-do]]
             [symbolic-values :as symbolic-values :refer [->RootVar]]]))

(defmacro ^:private assert-val
  ([val]
   (when *assert*
     `(let [val# ~val]
        (if (some? val#)
          val#
          (throw (new AssertionError (str "Assert failed: " (pr-str '~val))))))))
  ([val message]
   (when *assert*
     `(let [val# ~val]
        (if (some? val#)
          val#
          (throw (new AssertionError (str "Assert failed: " ~message))))))))

(def ^:private setup-symbolic-var
  "A symbolic value used to track the result of calling the `setup`
  function during command generation and execution."
  (->RootVar "setup"))

(defn ^:private generate-command-object
  "Generate the object for a command. This means first generating a
  command name using the spec's :model/generate-command function, then
  returning the full command object matching the generated command
  name. The generated name is `assoc`ed onto the command map under
  the :name key."
  [spec state]
  (gen/gen-fmap (fn [rose-tree]
                  (let [command-key (rose/root rose-tree)]
                    (assoc (assert-val (get (:commands spec) command-key)
                                       (str "Command " command-key " not found in :commands map"))
                           :name command-key)))
                ((:model/generate-command spec) state)))

;; Don't remove the `size` parameter to this function! It's there so
;; we can keep track of how "long" the command list is meant to be
;; without also influencing the "size" of the generated commands. You
;; can't use gen/sized because that would also influence the size of
;; the command generators themselves.
(defn ^:private generate-commands*
  "Returns a list of rose-trees within the monad of gen/gen-pure. "
  ([spec state size] (generate-commands* spec state size 0))
  ([spec state size count]
   (gen/frequency
    [[1 (gen/gen-pure nil)]
     [size (gen-do command <- (generate-command-object spec state)
                   args-rose <- (u/generate-args command state)
                   :let [args (rose/root args-rose)]
                   (if (u/check-precondition command state args)
                     (gen-do :let [result (->RootVar count)]
                             roses <- (generate-commands* spec
                                                          (u/model-make-next-state command state args result)
                                                          (dec size)
                                                          (inc count))
                             (gen/gen-pure (cons (rose/fmap (fn [args]
                                                              [result (cons command args)])
                                                            args-rose)
                                                 roses)))
                     (generate-commands* spec state count)))]])))

(defn ^:private concat-command-roses
  "Take a seq of rose trees and concatenate them. Create a vector from
  the roots along with a rose tree consisting of shrinking each
  element in turn."
  [roses]
  (if (seq roses)
    [(apply vector (map rose/root roses))
     (map concat-command-roses (rose/remove (vec roses)))]
    [[] []]))

(defn ^:private generate-commands
  "Generate a seq of commands from a spec and an initial state."
  [spec state]
  (gen/gen-bind (gen/sized #(generate-commands* spec state %))
                (fn [roses]
                  (gen/gen-pure (concat-command-roses roses)))))

(defn ^:private run-commands
  "Run a seq of commands against a live system."
  [spec command-list]
  (let [state-fn (or (:real/initial-state spec)
                     (:initial-state spec)
                     (constantly nil))
        setup-fn (:real/setup spec)
        setup-value (if setup-fn (setup-fn))
        results (if setup-fn
                  {setup-symbolic-var setup-value})
        state (if setup-fn
                (state-fn setup-value)
                (state-fn))
        command-results (r/run-commands command-list results state spec)]
    (if-let [f (:real/cleanup spec)]
      (f (last (r/extract-states command-results))))
    command-results))

(defn ^:private valid-commands?
  "Verify whether a given list of commands is valid (preconditions all
  return true, symbolic vars are all valid, etc.)."
  [spec command-list]
  (v/valid? command-list
            (if (:real/setup spec)
              #{setup-symbolic-var}
              #{})
            (let [initial-state-fn (or (:model/initial-state spec)
                                       (:initial-state spec)
                                       (constantly nil))]
              (if (:real/setup spec)
                (initial-state-fn setup-symbolic-var)
                (initial-state-fn)))))

(defn ^:private generate-valid-commands
  "Generate a set of valid commands from a specification"
  [spec]
  (->> (let [initial-state-fn (or (:model/initial-state spec)
                                  (:initial-state spec)
                                  (constantly nil))]
         (if (:real/setup spec)
           (initial-state-fn setup-symbolic-var)
           (initial-state-fn)))
       (generate-commands spec)
       (gen/such-that (partial valid-commands? spec))))

(defn ^{:deprecated "0.3.0"} reality-matches-model
  "Create a property which checks a given stateful-check
  specification."
  [spec]
  (for-all [commands (generate-valid-commands spec)]
    (let [command-results (run-commands spec commands)
          ex (r/extract-exception command-results)]
      (cond (r/passed? command-results) true
            ex (throw ex)
            :else false))))

(defn ^:private format-command [[sym-var [{name :name} _ args] :as cmd]]
  (str (pr-str sym-var) " = " (pr-str (cons name args))))

(defn ^:private print-command-results
  "Print out the results of a `run-commands` call. No commands are
  actually run, as the argument to this function contains the results
  for each individual command."
  ([results] (print-command-results results false))
  ([results stacktraces?]
   (try
     (doseq [[pre [type :as step]] (partition 2 1 results)]
       ;; get each state, plus the state before it (we can ignore the
       ;; first state because it's definitely not a terminal state
       ;; (:pass/:fail) and hence must have a following state which we
       ;; will see
       (case type
         :postcondition-check
         (let [[_ cmd _ _ _ _ result] step]
           (println "  " (format-command cmd) "\t=>" (pr-str result)))
         :fail
         (let [[_ ex] step
               [pre-type cmd] pre
               location (case pre-type
                          :precondition-check "checking precondition"
                          :run-command "executing command"
                          :postcondition-check "checking postcondition"
                          :next-state "making next state"
                          :next-command "checking spec postcondition")]
           (if-let [ex ^Throwable ex]
             (let [show-command? (and cmd (= :run-command pre-type))]
               (if show-command?
                 (println "  " (format-command cmd) "\t=!!>" (.getMessage ex)))
               (println "Exception thrown while" location
                        (if-not show-command? (str "- " (.getMessage ex))))
               (if stacktraces?
                 (.printStackTrace ex (java.io.PrintWriter. ^java.io.Writer *out*))))
             (println "Error while" location)))
         nil))
     (catch Throwable ex
       (println "Unexpected exception thrown in test runner -" (.getMessage ex))
       (.printStackTrace ex (java.io.PrintWriter. ^java.io.Writer *out*))))))

(defn print-test-results
  "Print the results of a test.check test in a more helpful form (each
  command with its corresponding output).

  This function will re-run both the failing test case and the shrunk
  failing test. This means that the commands will be run against the
  live system."
  [spec results {:keys [first-case? stacktraces?]}]
  (when-not (true? (:result results))
    (when first-case?
      (println "First failing test case:")
      (print-command-results (run-commands spec (-> results :fail first)) stacktraces?)
      (println "Shrunk:"))
    (print-command-results (run-commands spec (-> results :shrunk :smallest first)) stacktraces?)
    (println "Seed: " (:seed results))))

(def ^{:arglists '([specification]
                   [specification {:keys [num-tests max-size seed print-first-case? print-stacktraces?]}])}
  specification-correct?
  "This value is a dummy, just so you're aware it exists. It should
  only be used in an `is` form: (is (specification-correct? ...))" nil)

(defmethod t/assert-expr 'specification-correct?
  [msg [_ spec {:keys [num-tests max-size seed print-first-case? print-stacktraces?]}]]
  `(let [spec# ~spec
         results# (quick-check ~(or num-tests 100)
                               (reality-matches-model spec#)
                               :seed ~seed
                               :max-size ~(or max-size 200))]
     (if (true? (:result results#))
       (t/do-report {:type :pass,
                     :message ~msg,
                     :expected :pass,
                     :actual :fail})
       (t/do-report {:type :fail,
                     :message (str (if-let [msg# ~msg]
                                     (str msg# "\n"))
                                   (with-out-str (print-test-results spec# results#
                                                                     {:first-case? ~print-first-case?,
                                                                      :stacktraces? ~print-stacktraces?}))),
                     :expected :pass,
                     :actual :fail}))
     (:result results#)))
