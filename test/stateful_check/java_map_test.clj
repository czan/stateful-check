(ns stateful-check.java-map-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer [specification-correct?]]))

(def test-keys ["a" "b"])
;; ["" "a" "house" "tree" "λ"]

(def put-command
  {:model/args (fn [state]
                 [(:map state)
                  (gen/elements test-keys)
                  gen/int])
   :real/command (fn [^java.util.Map map key val]
                   (.put map key val))
   :next-state (fn [state [m k v] _]
                 (update state :contents assoc k v))})

(def get-command
  {:model/requires (fn [state]
                     (seq (:contents state)))
   :model/args (fn [state]
                 [(:map state)
                  (gen/elements test-keys)])
   :real/command (fn [^java.util.Map map key]
                   (.get map key))
   :real/postcondition (fn [prev-state _ [m k] val]
                         (= (get-in prev-state [:contents k])
                            val))})

(def java-map-specification
  {:commands {:put #'put-command
              :get #'get-command}
   :initial-state (fn [setup] {:map setup, :contents {}})
   :real/setup #(java.util.TreeMap.)})

(deftest ^:slow java-map-fails-concurrently
  (is (specification-correct? java-map-specification))
  (is (not (specification-correct? java-map-specification
                                   {:gen {:threads 2
                                          :max-length 5}
                                    :run {:max-tries 50}}))))
