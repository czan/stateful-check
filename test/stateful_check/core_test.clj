(ns stateful-check.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]))

(defn new-queue [] (atom (clojure.lang.PersistentQueue/EMPTY)))
(defn push-queue [queue val]
  (swap! queue conj val)
  nil)
(defn pop-queue [queue]
  (let [val (peek @queue)]
    (swap! queue pop)
    val))

(def queue-spec
  {:commands {:create {:next-state (fn [state _ result]
                                     {:queue result
                                      :elements []})
                       :real/command #'new-queue}
              :push {:model/args (fn [state]
                                   (gen/tuple (gen/return (:queue state))
                                              gen/nat))
                     :model/precondition (fn [state _]
                                           state)
                     :real/command #'push-queue
                     :next-state (fn [state [_ val] _]
                                   (assoc state
                                     :elements (conj (:elements state) val)))}
              :pop {:model/args (fn [state]
                                  (gen/return [(:queue state)]))
                    :model/precondition (fn [state _]
                                          (not (empty? (:elements state))))
                    :real/command #'pop-queue
                    :next-state (fn [state _ _]
                                  (assoc state
                                    :elements (vec (next (:elements state)))))
                    :real/postcondition (fn [state _ val]
                                          (= val (first (:elements state))))}}
   :generate-command (fn [state]
                       (gen/elements (if (nil? state)
                                       [:create]
                                       [:push :pop])))})

(defspec prop-queue
  (reality-matches-model? queue-spec))


(def global-state (atom #{}))
(def atomic-set-spec
  {:commands {:add {:model/args (fn [state]
                                  (gen/tuple gen/nat))
                    :model/precondition (fn [state _]
                                          (not (nil? state)))
                    :next-state (fn [state [arg] _]
                                  (conj state arg))
                    :real/command (partial swap! global-state conj)}

              :remove {:model/args (fn [state]
                                     (gen/tuple (gen/elements (vec state))))
                       :model/precondition (fn [state [arg]]
                                             (not (empty? state)))
                       :next-state (fn [state [arg] _]
                                     (disj state arg))
                       :real/command (partial swap! global-state disj)}

              :contains? {:model/args (fn [state]
                                        (gen/tuple (gen/one-of [(gen/elements (vec state))
                                                                gen/nat])))
                          :real/command (fn [val]
                                          (contains? @global-state val))
                          :real/postcondition (fn [state [value] result]
                                                (= (contains? state value) result))}

              :empty? {:real/command (fn [] (empty? @global-state))
                       :real/postcondition (fn [state _ result]
                                             (= (empty? state) result))}

              :empty {:next-state (fn [state _ _] #{})
                      :real/command (fn [] (reset! global-state #{}))}}
   :generate-command (fn [state]
                       (gen/elements (cond
                                      (nil? state) [:empty]
                                      (empty? state) [:add]
                                      :else [:add :remove :contains? :empty? :empty])))
   :cleanup (fn [state] (reset! global-state #{}))})

(defspec prop-atomic-set
  (reality-matches-model? atomic-set-spec))





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
                                           :real/postcondition (fn [state [ticker] result]
                                                                 (= result (inc (get state ticker))))}}
                  :generate-command (fn [state]
                                      (gen/elements (if (nil? state)
                                                      [:alloc-ticker]
                                                      [:alloc-ticker :zero :take-ticket])))})

(defspec prop-ticker
  (reality-matches-model? ticker-spec))













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
    :real/postcondition (fn [state [set item] result]
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
   :real/postcondition (fn [state [set item] result]
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
   :real/postcondition (fn [state [set] result]
                         (= result (empty? (alist-get state set))))})



(defn binary-set-command [combiner]
  {:model/args (fn [state]
                 (gen/tuple (gen/elements (map first state))
                            (gen/elements (map first state))))
   :next-state (fn [state [set1 set2] _]
                 (alist-update state set1
                               combiner (alist-get state set2)))
   :real/postcondition (fn [state [set1 set2] result]
                         (= result
                            (not= (combiner (alist-get state set1)
                                            (alist-get state set2))
                                  (alist-get state set1))))})

(def add-all-set-command
  (merge (binary-set-command clojure.set/union)
         {:real/command #(.addAll %1 %2)}))

(def remove-all-set-command
  (merge (binary-set-command clojure.set/difference)
         {:real/command #(.removeAll %1 %2)}))

(def retain-all-set-command
  (merge (binary-set-command clojure.set/intersection)
         {:real/command #(.retainAll %1 %2)}))


(def small-set-spec (let [command-map {:new (merge new-set-command
                                                   {:model/precondition (fn [state _] (nil? state))}) 
                                       :add add-set-command 
                                       :remove remove-set-command 
                                       :contains? contains?-set-command}]
                     {:commands command-map
                      :generate-command (fn [state]
                                          (gen/elements (if (nil? state)
                                                          [:new]
                                                          (keys command-map))))}))

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
                      :generate-command (fn [state]
                                          (gen/elements (if (nil? state)
                                                          [:new]
                                                          (keys command-map))))}))

(defspec prop-set
  (reality-matches-model? full-set-spec))

;; (prop-set)
;; (print-test-results full-set-spec (prop-set))



;; (:shrunk (quick-check 1000 (reality-matches-model? full-set-spec)
;;                       :seed 100000))

;; (:shrunk (quick-check 1000 (reality-matches-model? full-set-spec)))
