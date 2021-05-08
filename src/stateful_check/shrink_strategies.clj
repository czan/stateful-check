(ns stateful-check.shrink-strategies
  (:require [clojure.test.check.rose-tree :as rose]))

;; Within this file:
;;
;; - sequential is a vector of rose trees that represents the
;; sequential prefix of a command execution
;;
;; - parallel is a vector of vectors of rose trees that represents
;; several parallel threads of command execution

(defn remove-n-commands-from-sequential-prefix
  "Return a shrink strategy that will remove `n` commands from the
  sequential prefix."
  [n]
  (fn [sequential parallel]
    (map #(vector (vec %) parallel)
         (reduce (fn [sequentials _]
                   (mapcat rose/remove sequentials))
                 [sequential] (range n)))))

(defn remove-n-commands-from-parallel-threads
  "Return a shrink strategy that will remove `n` commands from a
  parallel thread, trying each thread in sequence."
  [n]
  (fn [sequential parallel]
    (map #(vector sequential %)
         (reduce (fn [parallels _]
                   (for [parallel   parallels
                         [i thread] (map vector (range) parallel)
                         thread     (rose/remove thread)]
                     (assoc parallel i thread)))
                 [parallel] (range n)))))

(defn pull-parallel-into-sequential
  "Return a shrink strategy that will move the first command from a
  parallel thread to be the last command in the sequential prefix.
  This may lead to further opportunities to shrink the sequential
  prefix."
  []
  (fn [sequential parallel]
    (for [[i thread] (map vector (range) parallel)]
      ;; pull one of the first parallel commands into the sequential prefix
      [(conj sequential (first thread))
       (update parallel i (comp vec next))])))
