(ns stateful-check.symbolic-values)

(defprotocol SymbolicValue
  (get-real-value [this real-values]
    "Lookup the value of this symbolic value in a real-values map")
  (valid? [this results]
    "Determine whether this symbolic value can be legally looked up in the results map"))



(deftype LookupVar [root-var key not-found]
  SymbolicValue
  (get-real-value [this real-values]
    (get (get-real-value root-var real-values)
         key
         not-found))
  (valid? [this results]
    (valid? root-var results))

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
  (get-real-value [this real-values]
    (get real-values this))
  (valid? [this results]
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
