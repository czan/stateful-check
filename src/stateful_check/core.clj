(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [stateful-check.core-utils :as utils]))

(defn ^{:deprecated "0.3.0"} reality-matches-model
  "Create a property which checks a given stateful-check
  specification."
  [spec]
  (utils/spec->property spec))

(defn ^{:deprecated "0.3.0",
        :doc (:doc (meta #'utils/print-test-results))
        :arglists (:arglists (meta #'utils/print-test-results))}
  print-test-results
  [spec results options]
  (utils/print-test-results spec results options))

(defn specification-correct?
  "Test whether or not the specification matches reality. This
  generates test cases and runs them."
  {:arglist (:arglist (meta #'utils/run-specification))}
  ([specification] (specification-correct? specification nil))
  ([specification {:keys [num-tests max-size seed] :as options}]
   (true? (:result (utils/run-specification specification options)))))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  `(let [spec# ~specification
         options# ~options
         results# (utils/run-specification spec# options#)]
     (if (true? (:result results#))
       (t/do-report {:type :pass,
                     :message ~msg,
                     :expected :pass,
                     :actual :pass})
       (t/do-report {:type :fail,
                     :message (with-out-str
                                (if-let [msg# ~msg]
                                  (println msg#))
                                (->> {:first-case? (:print-first-case? options#)
                                      :stacktraces? (:print-stacktraces? options#)}
                                     (utils/print-test-results spec# results#))),
                     :expected :pass,
                     :actual :fail}))
     (true? (:result results#))))
