(ns stateful-check.core
  (:require clojure.test.check
            [clojure.test.check
             [generators :as gen]
             [properties :refer [for-all]]
             [rose-tree :as rose]]
            [stateful-check
             [command-runner :as r]
             [command-verifier :as v]
             [gen :refer [gen-do]]
             [symbolic-values :as symbolic-values :refer [->RootVar SymbolicValue]]]))

;; The "setup" step uses a symbolic var, so we might as well just
;; allocate it once here and use it later.
(def ^:private setup-symbolic-var (->RootVar "setup"))

(defn generate-commands* [spec count size state]
  "Returns a list of rose-trees *within* the monad of gen/gen-pure"
  (gen/frequency
   [[1 (gen/gen-pure nil)]
    [size (gen-do com-rose <- ((:model/generate-command spec) state)
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
                    (gen-do :let [result (->RootVar count)
                                  next-state (if-let [f (or (:model/next-state command)
                                                            (:next-state command))]
                                               (f state args result)
                                               state)]
                            roses <- (generate-commands* spec (inc count) (dec size) next-state)
                            (gen/gen-pure (cons (rose/fmap (fn [args]
                                                             [result (cons com args)])
                                                           rose)
                                                roses)))
                    (generate-commands* spec count size state)))]]))

(defn shrink-commands-roses [roses]
  (if (seq roses)
    [(apply vector (map rose/root roses))
     (map shrink-commands-roses (rose/remove (vec roses)))]
    [[] []]))

(defn generate-commands [spec state]
  (gen/gen-bind (gen/sized (fn [size]
                             (generate-commands* spec 0 size state)))
                (fn [roses]
                  (gen/gen-pure (shrink-commands-roses roses)))))

(defn inline-commands [spec command-list]
  (map (fn [[sym-var [cmd & args]]]
         [sym-var (cons (-> (:commands spec)
                            (get cmd)
                            (assoc :name cmd))
                        args)])
       command-list))

(defn run-commands [spec command-list]
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
        command-results (r/run-commands (inline-commands spec command-list) results state)]
    (if-let [f (:real/cleanup spec)]
      (f (last (r/extract-states command-results))))
    command-results))

(defn valid-commands? [spec command-list]
  (v/valid? (inline-commands spec command-list)
            (if (:real/setup spec)
              #{setup-symbolic-var}
              #{})
            (let [initial-state-fn (or (:model/initial-state spec)
                                       (:initial-state spec)
                                       (constantly nil))]
              (if (:real/setup spec)
                (initial-state-fn setup-symbolic-var)
                (initial-state-fn)))))


(defn generate-valid-commands [spec]
  (->> (let [initial-state-fn (or (:model/initial-state spec)
                                  (:initial-state spec)
                                  (constantly nil))]
         (if (:real/setup spec)
           (initial-state-fn setup-symbolic-var)
           (initial-state-fn)))
       (generate-commands spec)
       (gen/such-that (partial valid-commands? spec))))

(defn reality-matches-model [spec]
  (for-all [commands (generate-valid-commands spec)]
    (let [command-results (run-commands spec commands)
          ex (r/extract-exception command-results)]
      (cond (r/passed? command-results) trueg
            ex (throw ex)
            :else false))))

(defn print-command-results [results]
  (try
    (doseq [[type :as step] results]
      (case type
        :postcondition-check
        (let [[_ [[sym-var [{name :name} _ args]]] _ prev-state _ result] step]
          (println \tab sym-var "=" (cons name args) "=>" result))
        :fail
        (if-let [ex (second step)]
          (println \tab ex)
          (println \tab "postcondition violation"))))
    (catch Throwable ex
      (println ex))))

(defn print-test-results [spec results]
  (when-not (true? (:result results))
    (println "\nFailing test case:")
    (print-command-results (run-commands spec (-> results :fail first)))
    (println "Shrunk:")
    (print-command-results (run-commands spec (-> results :shrunk :smallest first)))))
