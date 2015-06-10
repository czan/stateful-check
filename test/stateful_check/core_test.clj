(ns stateful-check.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.set :as set]
            [stateful-check.core :refer :all]))

(defn new-queue [] (atom (clojure.lang.PersistentQueue/EMPTY)))
(defn push-queue [queue val]
  (swap! queue conj val)
  nil)
(defn peek-queue [queue]
  (peek @queue))
(defn pop-queue [queue]
  (let [val (peek @queue)]
    (swap! queue pop)
    val))

(def queue-spec
  {:commands {:push {:model/args (fn [state]
                                   (gen/tuple (gen/return (:queue state))
                                              gen/nat))
                     :model/precondition (fn [state _]
                                           state)
                     :real/command #'push-queue
                     :next-state (fn [state [_ val] _]
                                   (assoc state
                                          :elements (conj (:elements state) val)))}
              :peek {:model/args (fn [state]
                                   (gen/return [(:queue state)]))
                     :model/precondition (fn [state _]
                                           (not (empty? (:elements state))))
                     :real/command #'peek-queue
                     :real/postcondition (fn [state _ args val]
                                           (= val (first (:elements state))))}
              :pop {:model/args (fn [state]
                                  (gen/return [(:queue state)]))
                    :model/precondition (fn [state _]
                                          (not (empty? (:elements state))))
                    :real/command #'pop-queue
                    :next-state (fn [state _ _]
                                  (assoc state
                                         :elements (vec (next (:elements state)))))
                    :real/postcondition (fn [state _ args val]
                                          (= val (first (:elements state))))}}
   :model/generate-command (fn [state]
                             (gen/elements [:push :pop]))
   :initial-state (fn [queue]
                    {:queue queue, :elements []})
   :real/setup #'new-queue})

(defspec prop-queue
  (reality-matches-model queue-spec))


(def global-state (atom #{}))
(def atomic-set-spec
  {:commands {:add {:model/args (fn [state]
                                  (gen/tuple gen/nat))
                    :next-state (fn [state [arg] _]
                                  (conj (or state #{}) arg))
                    :real/command #(swap! global-state conj %)}

              :remove {:model/args (fn [state]
                                     (gen/tuple (gen/elements (vec state))))
                       :model/precondition (fn [state [arg]]
                                             (not (empty? state)))
                       :next-state (fn [state [arg] _]
                                     (disj state arg))
                       :real/command #(swap! global-state disj %)}

              :contains? {:model/args (fn [state]
                                        (gen/tuple (gen/one-of [(gen/elements (vec state))
                                                                gen/nat])))
                          :real/command #(contains? @global-state %)
                          :real/postcondition (fn [state _ [value] result]
                                                (= (contains? state value) result))}

              :empty? {:real/command #(empty? @global-state)
                       :real/postcondition (fn [state _ _ result]
                                             (= (empty? state) result))}

              :empty {:next-state (fn [state _ _] #{})
                      :real/command (fn [] (reset! global-state #{}))}}
   
   :model/generate-command (fn [state]
                             (gen/elements (cond
                                             (empty? state) [:add]
                                             :else [:add :remove :contains? :empty? :empty])))
   
   :initial-state (fn [_]
                    #{})
   :real/setup #(reset! global-state #{})})

(defspec prop-atomic-set
  (reality-matches-model atomic-set-spec))





(defn ticker-init [] (atom 0))
(defn ticker-zero [ticker] (reset! ticker 0))
(defn ticker-take [ticker] (swap! ticker inc))

(def ticker-spec {:commands {:alloc-ticker {:model/next-state (fn [state _ ticker]
                                                                (assoc state ticker 0))
                                            :real/next-state (fn [state _ ticker] 
                                                               (assoc state ticker 0))
                                            :real/command ticker-init}

                             :zero {:model/args (fn [state]
                                                  (gen/tuple (gen/elements (keys state))))
                                    :model/precondition (fn [state _] state)
                                    :next-state (fn [state [ticker] _]
                                                  (assoc state ticker 0))
                                    :real/command ticker-zero}
                             
                             :take-ticket {:model/args (fn [state]
                                                         (gen/tuple (gen/elements (keys state))))
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

(defspec prop-ticker
  (reality-matches-model ticker-spec))













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
  (gen/tuple (gen/elements (map first state))
             gen/int))
(defn set-update-op [action]
  {:model/args set-and-item
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
  {:model/args set-and-item
   :real/command #(.contains %1 %2)
   :real/postcondition (fn [state _ [set item] result]
                         (= result (contains? (alist-get state set) item)))})



(def clear-set-command
  {:model/args (fn [state]
                 (gen/tuple (gen/elements (map first state))))
   :next-state (fn [state [set] _]
                 (alist-update state set (constantly #{})))
   :real/command #(.clear %1)})

(def empty?-set-command
  {:model/args (fn [state]
                 (gen/tuple (gen/elements (map first state))))
   :real/command #(.isEmpty %1)
   :real/postcondition (fn [state _ [set] result]
                         (= result (empty? (alist-get state set))))})



(defn binary-set-command [combiner]
  {:model/args (fn [state]
                 (gen/tuple (gen/elements (map first state))
                            (gen/elements (map first state))))
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


(def small-set-spec (let [command-map {:add add-set-command 
                                       :remove remove-set-command 
                                       :contains? contains?-set-command}]
                      {:commands command-map
                       :model/generate-command (fn [state]
                                                 (gen/elements (keys command-map)))
                       :initial-state (fn [set] [[set #{}]])
                       :real/setup #(java.util.HashSet.)}))

(defspec prop-small-set
  (reality-matches-model small-set-spec))

(def full-set-spec (let [command-map {:new new-set-command
                                      :add add-set-command
                                      :remove remove-set-command 
                                      :contains? contains?-set-command
                                      :clear clear-set-command
                                      :empty? empty?-set-command
                                      :add-all add-all-set-command
                                      :remove-all remove-all-set-command
                                      :retain-all retain-all-set-command}]
                     {:commands command-map
                      :model/generate-command (fn [state]
                                                (gen/elements (if (nil? state)
                                                                [:new]
                                                                (keys command-map))))}))

(defspec prop-full-set
  (reality-matches-model full-set-spec))
