(ns stateful-check.java-map-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer [specification-correct?]]))

(def test-keys ["a" "b"])
;; ["" "a" "house" "tree" "λ"]

(def new-command
  {:model/requires (fn [states]
                     (empty? states))
   :real/command (fn [] (java.util.TreeMap.))
   :next-state (fn [states _ result]
                 (conj states [result nil]))})

(def put-command
  {:model/requires (fn [states] (seq states))
   :model/args (fn [states]
                 [(gen/elements (map first states))
                  (gen/elements test-keys)
                  gen/int])
   :real/command (fn [^java.util.Map map key val]
                   (.put map key val))
   :next-state (fn [states [m k v] _]
                 (map (fn [[map contents]]
                        (if (identical? m map)
                          [map (assoc contents k v)]
                          [map contents]))
                      states))})

(def get-command
  {:model/requires (fn [states] (and (seq states)
                                    (seq (keys (apply merge (map second states))))))
   :model/args (fn [states]
                 [(gen/elements (map first states))
                  (gen/elements test-keys)])
   :real/command (fn [^java.util.Map map key]
                   (.get map key))
   :real/postcondition (fn [prev-state _ [m k] val]
                         (= (get (some (fn [[map contents]]
                                         (when (identical? m map)
                                           contents))
                                       prev-state)
                                 k)
                            val))})

(def java-map-specification
  {:commands {:new #'new-command
              :put #'put-command
              :get #'get-command}})

(deftest ^:slow java-map-fails-concurrently
  (is (specification-correct? java-map-specification))
  (is (not (specification-correct? java-map-specification
                                   {:gen {:threads 2
                                          :max-length 5}
                                    :run {:max-tries 50}
                                    :report {:first-case? true}}))))
