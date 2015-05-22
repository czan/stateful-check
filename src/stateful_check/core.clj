(ns stateful-check.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.rose-tree :as rose]
            [clojure.walk :as walk]
            [stateful-check.gen :refer [gdo gen-do]]
            [stateful-check.symbolic-values :refer [SymbolicValue ->RootVar get-real-value] :as symbolic-values]))

(def init-symbolic-var (->RootVar "setup"))

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


(defn remove-chunks
  [fraction roses]
  (let [num (count roses)
        size (/ num fraction)]
    (for [counter (range fraction)
          :let [slice-to-drop (dec (- fraction counter))]]
      (cond
       (zero? slice-to-drop) (drop size
                                   roses)
       (= slice-to-drop (dec fraction)) (take (* size slice-to-drop)
                                              roses)
       :else (concat (take (* size slice-to-drop)
                           roses)
                     (drop (* size (inc slice-to-drop))
                           roses))))))

(defn sublist-roses [roses]
  (concat (let [num (count roses)]
            (->> (iterate inc 2)
                 (take-while #(<= % num))
                 (mapcat #(remove-chunks % roses))))
          (rose/permutations (vec roses))))

(defn shrink-commands-roses [roses]
  (if (seq roses)
    [(apply vector (map rose/root roses))
     (map shrink-commands-roses (sublist-roses roses))]
    [[] []]))

(defn generate-commands [spec state]
  (gen/gen-bind (gen/sized (fn [size]
                             (generate-commands* spec 0 size state)))
                (fn [roses]
                  (gen/gen-pure (shrink-commands-roses roses)))))

(defmacro with-rethrown-state [state & body]
  `(try ~@body
        (catch Throwable ex#
          (throw (ex-info "Something was thrown" {:state ~state} ex#)))))

(defn run-commands
  ([spec generated-commands]
   (run-commands spec generated-commands (fn [& args])))
  ([spec generated-commands println] 
   (try
     (let [initial-state-fn (or (:real/initial-state spec)
                                (:initial-state spec)
                                (constantly nil))
           setup-fn (:real/setup spec)
           setup-value (if setup-fn (setup-fn))
           initial-state (if setup-fn
                           (initial-state-fn setup-value)
                           (initial-state-fn))
           initial-results (if setup-fn
                             {init-symbolic-var setup-value}
                             {})
           [state results] (reduce (fn [[state results] [result-var [com & raw-args]]]
                                     (let [command (get (:commands spec) com)
                                           args (walk/prewalk (fn [value]
                                                                (if (satisfies? SymbolicValue value)
                                                                  (get-real-value value results)
                                                                  value))
                                                              raw-args)
                                           result (with-rethrown-state state
                                                    (assert (:real/command command) (str "Command " com " does not have a :real/command function"))
                                                    (apply (:real/command command) args))
                                           old-state state
                                           state (if-let [f (or (:real/next-state command)
                                                                (:next-state command))]
                                                   (with-rethrown-state state
                                                     (f old-state args result))
                                                   state)
                                           passed? (if-let [f (:real/postcondition command)]
                                                     (with-rethrown-state state
                                                       (f old-state state args result))
                                                     true)
                                           passed-spec? (if (not passed?)
                                                          true
                                                          (if-let [f (:real/postcondition spec)]
                                                            (with-rethrown-state state
                                                              (f state))
                                                            true))]
                                       (println "  " result-var "=" (cons com raw-args) "\t;=>" result)
                                       (cond
                                        (not passed?) (do (println "   !! Command postcondition failed !!")
                                                          (reduced [state nil]))
                                        (not passed-spec?) (do (println "   !! Specification postcondition failed !!")
                                                               (reduced [state nil]))
                                        :else [state (assoc results result-var result)])))
                                   [initial-state initial-results]
                                   generated-commands)]
       (if-let [f (:real/cleanup spec)]
         (f state))
       (boolean results))
     (catch clojure.lang.ExceptionInfo ex
       (println "  !! Exception thrown !!")
       (if-let [f (:real/cleanup spec)]
         (f (get (ex-data ex) :state)))
       (throw (.getCause ex))))))

(defn valid-commands? [spec command-list]
  (let [initial-state (let [initial-state-fn (or (:model/initial-state spec)
                                                 (:initial-state spec)
                                                 (constantly nil))]
                        (if (:real/setup spec)
                          (initial-state-fn init-symbolic-var)
                          (initial-state-fn)))
        initial-results (if (:real/setup spec)
                          #{init-symbolic-var}
                          #{})]
    (first (reduce (fn [[valid? state results] [result-var [com & args]]]
                     (let [command (get (:commands spec) com)
                           precondition-passed? (and
                                                 (every? (fn [arg]
                                                           (if (satisfies? SymbolicValue arg)
                                                             (symbolic-values/valid? arg results)
                                                             true))
                                                         args)
                                                 (if-let [f (:model/precondition command)]
                                                   (f state args)
                                                   true))]
                       (if precondition-passed?
                         (if-let [f (or (:model/next-state command)
                                        (:next-state command))]
                           [true (f state args result-var) (conj results result-var)]
                           [true state results])
                         (reduced [false nil nil]))))
                   [true initial-state initial-results] command-list))))



(defn reality-matches-model [spec]
  (for-all [commands (gen/such-that (partial valid-commands? spec)
                                    (generate-commands spec (let [initial-state-fn (or (:model/initial-state spec)
                                                                                       (:initial-state spec)
                                                                                       (constantly nil))]
                                                              (if (:real/setup spec)
                                                                (initial-state-fn init-symbolic-var)
                                                                (initial-state-fn)))))]
    (run-commands spec commands)))

(defn print-test-results [spec results]
  (when-not (true? (:result results))
    (println "\nFailing test case:")
    (try
      (run-commands spec (-> results :fail first) println)
      (catch Throwable ex))
    (println "Shrunk:")
    (try
      (run-commands spec (-> results :shrunk :smallest first) println)
      (catch Throwable ex))))



