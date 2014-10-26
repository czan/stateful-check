(ns test-state-m.gen
  (:require [clojure.test.check.generators :as gen]))

(defn is-let? [forms]
  (and (keyword? (first forms))
       (= (namespace (first forms)) nil)
       (= (name (first forms)) "let")))

(defn is-bind? [forms]
  (and (symbol? (second forms))
       (= (namespace (second forms)) nil)
       (= (name (second forms)) "<-")))

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

(defmacro gdo [& forms]
  (gdo* forms `gen/bind))

(defmacro gen-do [& forms]
  (gdo* forms `gen/gen-bind))
