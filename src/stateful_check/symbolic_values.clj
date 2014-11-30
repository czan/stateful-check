(ns stateful-check.symbolic-values)

(defprotocol SymbolicValue
  (get-real-value [this real-values]
    "Lookup the value of this symbolic value in a real-values map"))



(deftype LookupVar [root-var key not-found]
  SymbolicValue
  (get-real-value [this real-values]
    (get (get-real-value root-var real-values)
         key
         not-found)))

(defmethod print-method LookupVar
  [^LookupVar v, ^java.io.Writer writer]
  (.write writer "(get ")
  (print-method (.-root-var v) writer)
  (.write writer " ")
  (print-method (.-key v) writer)
  (.write writer " ")
  (print-method (.-not-found v) writer)
  (.write writer ")"))



(deftype RootVar [num]
  SymbolicValue
  (get-real-value [this real-values]
    (get real-values this))

  clojure.lang.ILookup
  (valAt [this key]
    (->LookupVar this key nil))
  (valAt [this key not-found]
    (->LookupVar this key not-found)))

(defmethod print-method RootVar
  [^RootVar v, ^java.io.Writer writer]
  (.write writer (str "#<" (.-num v) ">")))
