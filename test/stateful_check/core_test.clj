(ns stateful-check.core-test
  (:require [clojure
             [set :as set]
             [test :refer :all]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

(defn ticker-init [] (atom 0))
(defn ticker-zero [ticker] (reset! ticker 0))
(defn ticker-take [ticker] (swap! ticker inc))

(def ticker-spec {:commands {:alloc-ticker {:model/next-state (fn [state _ ticker]
                                                                (assoc state ticker 0))
                                            :real/next-state (fn [state _ ticker] 
                                                               (assoc state ticker 0))
                                            :real/command ticker-init}

                             :zero {:model/args (fn [state]
                                                  [(gen/elements (keys state))])
                                    :model/precondition (fn [state _] state)
                                    :next-state (fn [state [ticker] _]
                                                  (assoc state ticker 0))
                                    :real/command ticker-zero}
                             
                             :take-ticket {:model/args (fn [state]
                                                         [(gen/elements (keys state))])
                                           :model/precondition (fn [state _] state)
                                           :next-state (fn [state [ticker] _]
                                                         (assoc state
                                                                ticker (inc (get state ticker))))
                                           :real/command ticker-take
                                           :real/postcondition (fn [state _ [ticker] result]
                                                                 (= result (inc (get state ticker))))}}
                  :model/generate-command (fn [state]
                                            (gen/elements (if (nil? state)
                                                            [:alloc-ticker]
                                                            [:alloc-ticker :zero :take-ticket])))})

(deftest ticker-test
  (is (specification-correct? ticker-spec)))













(defn alist-get [alist key]
  (some (fn [[k v]]
          (if (identical? k key)
            v))
        alist))

(defn alist-update [alist key f & args]
  (mapv (fn [[k v]]
          (if (identical? k key)
            [k (apply f v args)]
            [k v]))
        alist))

(def new-set-command
  {:next-state (fn [state _ result]
                 (if state
                   (conj state [result #{}])
                   [[result #{}]]))
   :real/command #(java.util.HashSet. [])})

(defn set-and-item [state]
  [(gen/elements (map first state))
   gen/int])
(defn set-update-op [action]
  {:model/requires (fn [state]
                     (seq state))
   :model/args set-and-item
   :next-state (fn [state [set item] _]
                 (alist-update state set action item))
   :real/postcondition (fn [state _ [set item] result]
                         (= result
                            (not= (alist-get state set)
                                  (action (alist-get state set) item))))})

(def add-set-command
  (merge (set-update-op conj)
         {:real/command #(.add %1 %2)}))

(def remove-set-command
  (merge (set-update-op disj)
         {:real/command #(.remove %1 %2)}))

(def contains?-set-command
  {:model/requires (fn [state]
                     (seq state))
   :model/args set-and-item
   :real/command #(.contains %1 %2)
   :real/postcondition (fn [state _ [set item] result]
                         (= result (contains? (alist-get state set) item)))})



(def clear-set-command
  {:model/requires (fn [state]
                     (seq state))
   :model/args (fn [state]
                 [(gen/elements (map first state))])
   :next-state (fn [state [set] _]
                 (alist-update state set (constantly #{})))
   :real/command #(.clear %1)})

(def empty?-set-command
  {:model/requires (fn [state]
                     (seq state))
   :model/args (fn [state]
                 [(gen/elements (map first state))])
   :real/command #(.isEmpty %1)
   :real/postcondition (fn [state _ [set] result]
                         (= result (empty? (alist-get state set))))})



(defn binary-set-command [combiner]
  {:model/requires (fn [state]
                     (seq state))
   :model/args (fn [state]
                 [(gen/elements (map first state))
                  (gen/elements (map first state))])
   :next-state (fn [state [set1 set2] _]
                 (alist-update state set1
                               combiner (alist-get state set2)))
   :real/postcondition (fn [state _ [set1 set2] result]
                         (= result
                            (not= (combiner (alist-get state set1)
                                            (alist-get state set2))
                                  (alist-get state set1))))})

(def add-all-set-command
  (merge (binary-set-command set/union)
         {:real/command #(.addAll %1 %2)}))

(def remove-all-set-command
  (merge (binary-set-command set/difference)
         {:real/command #(.removeAll %1 %2)}))

(def retain-all-set-command
  (merge (binary-set-command set/intersection)
         {:real/command #(.retainAll %1 %2)}))


(def small-set-spec {:commands {:add add-set-command
                                :remove remove-set-command
                                :contains? contains?-set-command}
                     :initial-state (fn [set] [[set #{}]])
                     :real/setup #(java.util.HashSet.)})

(deftest small-set-test
  (is (specification-correct? small-set-spec)))

(def full-set-spec {:commands {:new new-set-command
                               :add add-set-command
                               :remove remove-set-command
                               :contains? contains?-set-command
                               :clear clear-set-command
                               :empty? empty?-set-command
                               :add-all add-all-set-command
                               :remove-all remove-all-set-command
                               :retain-all retain-all-set-command}})

(deftest full-set-test
  (is (specification-correct? full-set-spec)))
