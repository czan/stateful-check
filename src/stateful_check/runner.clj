(ns stateful-check.runner
  (:require [stateful-check.command-utils :as u]
            [stateful-check.symbolic-values :as sv]))

(defn make-sequential-runners [cmd-objs]
  (mapv (fn [[handle cmd-obj & args]]
          (if-let [function (:command cmd-obj)]
            [handle #(apply function (sv/get-real-value args %))]
            (throw (AssertionError. (str "No :command function found for "
                                         (:name cmd-obj)
                                         " command")))))
        cmd-objs))

(defrecord CaughtException [exception])

(defn run-sequential-runners [runners bindings assume-immutable-results]
  (reduce (fn [[bindings trace str-trace] [handle f]]
            (try
              (let [value (f bindings)]
                [(assoc bindings handle value)
                 (conj trace value)
                 (if assume-immutable-results
                   (conj str-trace nil)
                   (conj str-trace (pr-str value)))])
              (catch Exception exception
                (reduced [bindings
                          (conj trace (->CaughtException exception))
                          (if assume-immutable-results
                            (conj str-trace nil)
                            (conj str-trace exception))
                          exception]))))
          [bindings [] []]
          runners))

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

(defn runners->results [{:keys [sequential parallel]} bindings timeout-ms assume-immutable-results]
  (try
    (with-timeout timeout-ms
      (let [[bindings trace str-trace exception] (run-sequential-runners sequential bindings assume-immutable-results)
            latch (java.util.concurrent.atomic.AtomicBoolean. true)
            futures (when-not exception
                      (mapv #(future
                               (while (.get latch)
                                 ;; spin until all the futures have been
                                 ;; created (this is probably unnecessary,
                                 ;; but just in case)
                                 )
                               (run-sequential-runners % bindings assume-immutable-results))
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

(defn failure-message
  "Return a vector of [handle message] representing which command
  failed, and why. Returns nil if no command has failed."
  [cmds-and-traces state bindings]
  (first (reduce (fn [[_ state bindings] [[handle cmd-obj & args] result]]
                   (if (instance? CaughtException result)
                     (reduced [[handle "Unexpected exception thrown."]])
                     (let [replaced-args (sv/get-real-value args bindings)
                           next-state (u/make-next-state cmd-obj state replaced-args result)]
                       (if-let [message (u/check-postcondition cmd-obj state next-state replaced-args result)]
                         (reduced [[handle message]])
                         [nil
                          next-state
                          (assoc bindings handle result)]))))
                 [nil state bindings] cmds-and-traces)))
