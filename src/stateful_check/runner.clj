(ns stateful-check.runner
  (:require [stateful-check.command-utils :as u]
            [stateful-check.symbolic-values :as sv]))

;; TODO: handling exceptions is currently very broken. VERY BROKEN. (as in, there is no handling for them)
(defn- run-sequential-commands [cmd-objs state bindings]
  (reduce (fn [[state trace bindings str-trace] [handle cmd-obj & args]]
            (let [replaced-args (map (fn [value]
                                       (if (satisfies? sv/SymbolicValue value)
                                         (sv/get-real-value value bindings)
                                         value))
                                     args)
                  value (u/run-command cmd-obj replaced-args)
                  next-state (u/real-make-next-state cmd-obj state replaced-args value)]
              [next-state
               (conj trace value)
               (assoc bindings handle value)
               (conj str-trace (pr-str value))]))
          [state [] bindings []] cmd-objs))

(defn run-commands [{:keys [sequential parallel]} state bindings]
  (let [[state trace bindings str-trace exception?] (run-sequential-commands sequential state bindings)
        latch (java.util.concurrent.atomic.AtomicBoolean. true)
        futures (when-not exception?
                  (mapv #(future
                           (while (.get latch)
                             ;; spinlock
                             )
                           (run-sequential-commands % state bindings))
                        parallel))]
    (.set latch false)
    {:sequential trace
     :sequential-strings str-trace
     :parallel (mapv (comp second deref) futures)
     :parallel-strings (mapv (comp #(nth % 3) deref) futures)}))

(defn valid-execution? [cmds-and-traces state bindings]
  (boolean (reduce (fn [[state bindings] [[handle cmd-obj & args] result]]
                     (let [replaced-args (map (fn [value]
                                                (if (satisfies? sv/SymbolicValue value)
                                                  (sv/get-real-value value bindings)
                                                  value))
                                              args)
                           next-state (u/real-make-next-state cmd-obj state replaced-args result)]
                       (if (u/check-postcondition cmd-obj state next-state replaced-args result)
                         [next-state
                          (assoc bindings handle result)]
                         (reduced false))))
                   [state bindings] cmds-and-traces)))
