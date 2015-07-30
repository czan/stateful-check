(ns stateful-check.generator-utils
  (:require [clojure.test.check.generators :as gen]))

(defn is-let? [forms]
  (and (keyword? (first forms))
       (nil? (namespace (first forms)))
       (= (name (first forms)) "let")))

(defn is-bind? [forms]
  (and (symbol? (second forms))
       (nil? (namespace (second forms)))
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

(defmacro gen-do [& forms]
  (gdo* forms `gen/gen-bind))
