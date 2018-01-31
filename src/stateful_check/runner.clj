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
          (let [replacer (args-replacer args)]
            [handle (fn [bindings]
                      (u/run-command cmd-obj (replacer [] bindings)))]))
        cmd-objs))

(defn run-sequential-runners [runners bindings]
  (reduce (fn [[bindings trace str-trace] [handle f]]
            (try
              (let [value (f bindings)]
                [(assoc bindings handle value)
                 (conj trace value)
                 (conj str-trace (pr-str value))])
              (catch Exception exception
                (reduced [bindings
                          (conj trace exception)
                          (conj str-trace (pr-str exception))
                          exception]))))
          [bindings [] []] runners))

(defn commands->runners [{:keys [sequential parallel]}]
  {:sequential (make-sequential-runners sequential)
   :parallel (mapv make-sequential-runners parallel)})

(defn runners->results [{:keys [sequential parallel]} bindings]
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
    (.set latch false)
    {:sequential trace
     :sequential-strings str-trace
     :parallel (mapv (comp second deref) futures)
     :parallel-strings (mapv (comp #(nth % 2) deref) futures)}))

(defn valid-execution? [cmds-and-traces state bindings]
  (boolean (reduce (fn [[state bindings] [[handle cmd-obj & args] result]]
                     (let [replacer (args-replacer args)
                           replaced-args (replacer [] bindings)
                           next-state (u/real-make-next-state cmd-obj state replaced-args result)]
                       (if (u/check-postcondition cmd-obj state next-state replaced-args result)
                         [next-state
                          (assoc bindings handle result)]
                         (reduced false))))
                   [state bindings] cmds-and-traces)))
