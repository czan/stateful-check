(ns stateful-check.generate-command-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

(def spec
  {:commands {:run {:requires (constantly false)
                    :command (constantly nil)}}
   :initial-state (constantly nil)})

(deftest default-generate-commands
  (is
   (try
     (specification-correct? spec)
     false
     (catch AssertionError e
       true))))

(deftest custom-generate-commands
  (is
   (try
     (specification-correct?
      (assoc spec
             :generate-command (constantly (gen/return :run))))
     false
     (catch AssertionError e
       true))))
