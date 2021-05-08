(ns stateful-check.symbolic-values
  (:require [clojure.walk :as walk]))

(defprotocol SymbolicValue
  (get-real-value* [this real-values]
    "Lookup the value of this symbolic value in a real-values map")
  (valid?* [this results]
    "Detemine whether this symbolic value can be legally looked up in the results map"))

(defn get-real-value [argument real-values]
  ;; This is a prewalk, in case substituting a symbolic value leads to
  ;; more opportunities to substitute symbolic values.
  (walk/prewalk (fn [value]
                  (if (satisfies? SymbolicValue value)
                    (get-real-value* value real-values)
                    value))
                argument))

(defonce invalid-exception (Exception.))

(defn valid? [argument results]
  (try
    ;; This is a postwalk, so returning nil from the walk function
    ;; doesn't prune the tree.
    (walk/postwalk (fn [value]
                    (when (and (satisfies? SymbolicValue value)
                               (not (valid?* value results)))
                      (throw invalid-exception)))
                  argument)
    true
    (catch Exception e
      (if (identical? e invalid-exception)
        false
        (throw e)))))



(deftype LookupVar [root-var key not-found]
  SymbolicValue
  (get-real-value* [this real-values]
    (get (get-real-value* root-var real-values)
         key
         not-found))
  (valid?* [this results]
    (valid?* root-var results))

  Object
  (equals [this other]
    (and (instance? LookupVar other)
         (= (.-root-var this)
            (.-root-var ^LookupVar other))
         (= (.-key this)
            (.-key ^LookupVar other))
         (= (.-not-found this)
            (.-not-found ^LookupVar other))))
  (hashCode [this]
    (java.util.Objects/hash (into-array Object [root-var key not-found])))

  clojure.lang.ILookup
  (valAt [this key]
    (LookupVar. this key nil))
  (valAt [this key not-found]
    (LookupVar. this key not-found)))

(defmethod print-method LookupVar
  [^LookupVar v, ^java.io.Writer writer]
  (.write writer "(get ")
  (print-method (.-root-var v) writer)
  (.write writer " ")
  (print-method (.-key v) writer)
  (when-not (nil? (.-not-found v))
    (.write writer " ")
    (print-method (.-not-found v) writer))
  (.write writer ")"))



(deftype RootVar [name]
  SymbolicValue
  (get-real-value* [this real-values]
    (get real-values this))
  (valid?* [this results]
    (contains? results this))

  Object
  (equals [this other]
    (and (instance? RootVar other)
         (= (.-name this)
            (.-name ^RootVar other))))
  (hashCode [this]
    (.hashCode name))

  clojure.lang.ILookup
  (valAt [this key]
    (->LookupVar this key nil))
  (valAt [this key not-found]
    (->LookupVar this key not-found)))

(defmethod print-method RootVar
  [^RootVar v, ^java.io.Writer writer]
  (.write writer (str "#<" (.-name v) ">")))
