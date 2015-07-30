(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [stateful-check.core-utils :as utils]))

(defn ^{:deprecated "0.3.0"} reality-matches-model
  "Create a property which checks a given stateful-check
  specification."
  [spec]
  (utils/spec->property spec))

(defn ^{:deprecated "0.3.0"} print-test-results
  [spec results {:keys [first-case? stacktraces?]}]
  (utils/print-test-results spec results {:first-case first-case?, :stacktraces? stacktraces?}))

(def ^{:arglists '([specification]
                   [specification {:keys [num-tests max-size seed print-first-case? print-stacktraces?]}])}
  specification-correct?
  "This value is a dummy, just so you're aware it exists. It should
  only be used in an `is` form: (is (specification-correct? ...))" nil)

(defmethod t/assert-expr 'specification-correct?
  [msg [_ spec {:keys [num-tests max-size seed print-first-case? print-stacktraces?]}]]
  `(let [spec# ~spec
         results# (quick-check ~(or num-tests 100)
                               (utils/spec->property spec#)
                               :seed ~seed
                               :max-size ~(or max-size 200))]
     (if (true? (:result results#))
       (t/do-report {:type :pass,
                     :message ~msg,
                     :expected :pass,
                     :actual :pass})
       (t/do-report {:type :fail,
                     :message (str (if-let [msg# ~msg]
                                     (str msg# "\n"))
                                   (with-out-str (utils/print-test-results spec# results#
                                                                           {:first-case? ~print-first-case?,
                                                                            :stacktraces? ~print-stacktraces?}))),
                     :expected :pass,
                     :actual :fail}))
     (:result results#)))
