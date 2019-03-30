(ns stateful-check.runner
  (:require [stateful-check.command-utils :as u]
            [stateful-check.symbolic-values :as sv]))

(defn- args-replacer [args]
  (if (empty? args)
    (fn [result bindings] result)
    (let [value (first args)
          value-f (if (satisfies? sv/SymbolicValue value)
                    (fn [bindings]
                      (sv/get-real-value value bindings))
                    (fn [bindings]
                      value))
          replacer-rest (args-replacer (rest args))]
      (fn [result bindings]
        (-> result
            (conj (value-f bindings))
            (replacer-rest bindings))))))

(defn make-sequential-runners [cmd-objs]
  (mapv (fn [[handle cmd-obj & args]]
          (let [replacer (args-replacer args)
                function (:command cmd-obj)]
            (if function
              [handle #(apply function (replacer [] %))]
              (throw (AssertionError. (str "No :command function found for "
                                           (:name cmd-obj)
                                           " command"))))))
        cmd-objs))

(defrecord CaughtException [exception])

(defn run-sequential-runners [runners bindings]
  (reduce (fn [[bindings trace str-trace] [handle f]]
            (try
              (let [value (f bindings)]
                [(assoc bindings handle value)
                 (conj trace value)
                 (conj str-trace (pr-str value))])
              (catch Exception exception
                (reduced [bindings
                          (conj trace (->CaughtException exception))
                          (conj str-trace exception)
                          exception]))))
          [bindings [] []] runners))

(defn commands->runners [{:keys [sequential parallel]}]
  {:sequential (make-sequential-runners sequential)
   :parallel (mapv make-sequential-runners parallel)})

(defmacro with-timeout [timeout-ms & body]
  `(let [timeout-ms# ~timeout-ms]
     (if (<= timeout-ms# 0)
       (do ~@body)
       (let [f# (future ~@body)
             v# (deref f# timeout-ms# ::timeout)]
         (if (= v# ::timeout)
           (do (future-cancel f#)
               (throw (InterruptedException. "Timed out")))
           v#)))))

(defn runners->results [{:keys [sequential parallel]} bindings timeout-ms]
  (try
    (with-timeout timeout-ms
      (let [[bindings trace str-trace exception] (run-sequential-runners sequential bindings)
            latch (java.util.concurrent.atomic.AtomicBoolean. true)
            futures (when-not exception
                      (mapv #(future
                               (while (.get latch)
                                 ;; spin until all the futures have been
                                 ;; created (this is probably unnecessary,
                                 ;; but just in case)
                                 )
                               (run-sequential-runners % bindings))
                            parallel))]
        (try
          (.set latch false)
          (let [values (mapv deref futures)]
            {:sequential trace
             :sequential-strings str-trace
             :parallel (mapv #(nth % 1) values)
             :parallel-strings (mapv #(nth % 2) values)})
          (catch InterruptedException ex
            (mapv future-cancel futures)))))
    (catch InterruptedException ex
      (throw (ex-info "Timed out"
                      {:sequential (mapv (constantly ::unevaluated) sequential)
                       :sequential-strings (mapv (constantly "???") sequential)
                       :parallel (mapv #(mapv (constantly ::unevaluated) %) parallel)
                       :parallel-strings (mapv #(mapv (constantly "???") %) parallel)})))))

(defn valid-execution? [cmds-and-traces state bindings]
  (boolean (reduce (fn [[state bindings] [[handle cmd-obj & args] result]]
                     (if (instance? CaughtException result)
                       (reduced false)
                       (let [replacer (args-replacer args)
                             replaced-args (replacer [] bindings)
                             next-state (u/make-next-state cmd-obj state replaced-args result)]
                         (if (u/check-postcondition cmd-obj state next-state replaced-args result)
                           [next-state
                            (assoc bindings handle result)]
                           (reduced false)))))
                   [state bindings] cmds-and-traces)))
