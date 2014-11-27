(ns stateful-check.core
  (:require [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.properties :refer [for-all]]
            [stateful-check.gen :refer [gdo gen-do]]))

(defrecord Var [num])
(defmethod print-method Var
  [v ^java.io.Writer writer]
  (.write writer (str "#<" (:num v) ">")))

(defn generate-commands* [spec count state]
  "Returns a list of rose-trees *within* the monad of gen/gen-pure"
  (gen/sized
   (fn [size]
     (gen/frequency
      [[1 (gen/gen-pure nil)]
       [size (gen-do com-rose <- ((:generate-command spec) state)
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
                       (gen-do :let [result (->Var count)
                                     next-state (if-let [f (or (:model/next-state command)
                                                               (:next-state command))]
                                                  (f state args result)
                                                  state)]
                               roses <- (gen/resize (dec size) (generate-commands* spec (inc count) next-state))
                               (gen/gen-pure (cons (rose/fmap (fn [args]
                                                                [result (cons com args)])
                                                              rose)
                                                   roses)))
                       (generate-commands* spec count state)))]]))))


(defn remove-chunks
  [fraction roses]
  (let [num (count roses)
        size (/ num fraction)]
    (for [counter (range fraction)
          :let [slice-to-drop (- fraction counter 1)]]
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
  (gen/gen-bind (generate-commands* spec 0 state)
                (fn [roses]
                  (gen/gen-pure (shrink-commands-roses roses)))))

(defn run-commands
  ([spec generated-commands]
   (run-commands spec generated-commands (fn [& args])))
  ([spec generated-commands println] 
   (let [[state result ex] (reduce (fn [[state results] [result-var [com & raw-args]]]
                                     (let [command (get (:commands spec) com)
                                           args (replace results raw-args)
                                           [result exception] (try [(do (assert (:real/command command) (str "Command " com " does not have a :real/command function"))
                                                                        (apply (:real/command command) args))
                                                                    nil]
                                                                   (catch Throwable ex
                                                                     [nil ex]))
                                           [passed? exception] (if exception
                                                                 [false exception]
                                                                 (if-let [f (:real/postcondition command)]
                                                                   (try
                                                                     [(f state args result)]
                                                                     (catch Throwable ex
                                                                       [false ex]))
                                                                   [true]))
                                           [state exception] (if-let [f (or (:real/next-state command)
                                                                            (:next-state command))]
                                                               (try
                                                                 [(f state args result) exception]
                                                                 (catch Throwable ex
                                                                   [state (or exception ex)]))
                                                               [state exception])]
                                       (println "  " result-var "=" (cons com raw-args) "\t;=>" result)
                                       (cond
                                        exception (do (println "  !! Exception thrown !!")
                                                      (reduced [state nil exception]))
                                        passed? [state (assoc results result-var result)]
                                        :else (do (println "   !! Postcondition failed !!")
                                                  (reduced [state nil exception])))
                                       (if (and passed? (not exception))
                                         [state (assoc results result-var result)]
                                         (reduced [state nil exception]))))
                                   [(if-let [f (:setup spec)]
                                      (f))
                                    {}]
                                   generated-commands)]
     (if-let [f (:cleanup spec)]
       (f state))
     (if ex
       (throw ex)
       (boolean result)))))

(defn valid-commands? [spec command-list]
  (first (reduce (fn [[valid? state results] [result-var [com & args]]]
                   (let [command (get (:commands spec) com)
                         precondition-passed? (and
                                               (every? (fn [arg]
                                                         (or (not (instance? Var arg))
                                                             (contains? results arg)))
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
                 [true nil #{}] command-list)))



(defn reality-matches-model? [spec]
  (for-all [commands (gen/such-that (partial valid-commands? spec)
                                    (generate-commands spec nil))]
    (run-commands spec commands)))

(defn print-test-results [spec results]
  (when-not (true? (:result results))
    (println "\nFailing test case:")
    (run-commands spec (-> results :fail first) println)
    (println "Shrunk:")
    (run-commands spec (-> results :shrunk :smallest first) println)))



