(ns stateful-check.generator-utils
  (:require [clojure.test.check.generators :as gen]))

(defn is-let? [forms]
  (= :let (first forms)))

(defn is-bind? [forms]
  (= '<- (second forms)))

(defn gdo* [forms bind]
  (cond
   (nil? (next forms)) (first forms)
   (is-let? forms) `(let ~(second forms) ~(gdo* (nnext forms) bind))
   (is-bind? forms) `(~bind ~(nth forms 2)
                               (fn [~(first forms)]
                                 ~(gdo* (nthnext forms 3) bind)))
   :else `(~bind ~(first forms)
                    (fn [_#]
                      ~(gdo* (next forms) bind)))))

(defmacro gen-do [& forms]
  (gdo* forms `gen/gen-bind))

(defn to-generator
  "Convert a value into a generator, recursively. This means:
    + generator? -> the value
    + sequential? -> gen/tuple with each sub-value already processed
    + map? -> gen/hash-map with each value (not keys) already processed
    + otherwise -> gen/return the value"
  [value]
  (cond (gen/generator? value) value
        (sequential? value) (apply gen/tuple (map to-generator value))
        (map? value) (apply gen/hash-map (mapcat (fn [[k v]]
                                                   [k (to-generator v)])
                                                 value))
        :else (gen/return value)))
