(ns stateful-test.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [stateful-test.core :refer :all]))

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
   :cleanup #(reset! global-state #{})})
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




(def set-spec {:commands {:new {:model/precondition (fn [[set _] _]
                                                      (nil? set))
                                :next-state (fn [_ _ set]
                                              [set #{}])
                                :real/command #(java.util.HashSet.)}

                          :add {:model/args (fn [[set _]]
                                              (gen/tuple (gen/return set) gen/int))
                                :model/precondition (fn [[set _] _]
                                                      (not (nil? set)))
                                :next-state (fn [[set elements] [_ arg] _]
                                              [set (conj elements arg)])
                                :real/command #(.add %1 %2)}

                          :remove {:model/args (fn [[set _]]
                                                 (gen/tuple (gen/return set) gen/int))
                                   :model/precondition (fn [[set _] _]
                                                         (not (nil? set)))
                                   :next-state (fn [[set elements] [_ arg] _]
                                                 [set (disj elements arg)])
                                   :real/command #(.remove %1 %2)}

                          :contains? {:model/args (fn [[set _]]
                                                    (gen/tuple (gen/return set) gen/int))
                                      :model/precondition (fn [[set _] _]
                                                            (not (nil? set)))
                                      :real/command #(.contains %1 %2)
                                      :real/postcondition (fn [[set elements] [_ arg] result]
                                                            (= result
                                                               (contains? elements arg)))}}
               :generate-command (fn [[set _]]
                                   (gen/elements (if (nil? set)
                                                   [:new]
                                                   [:add :contains? :remove])))})

(defspec prop-set
  (reality-matches-model? set-spec))

