# stateful-check

A Clojure library designed to help with testing stateful systems
within [`test.check`](https://github.com/clojure/test.check/).

## Example

As an example, let's test a mutable queue implementation held within
an atom:

    (defn new-queue [] (atom (clojure.lang.PersistentQueue/EMPTY)))
    (defn push-queue [queue val]
      (swap! queue conj val)
      nil)
    (defn pop-queue [queue]
      (let [val (peek @queue)]
        (swap! queue pop)
        val))

Our specification could look like this:

    (require '[clojure.test.check.generators :as gen])
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

Note that we've made the decision to explicitly instantiate our queue
within our test. We could also have used a globally defined queue and
reset it within the `:cleanup` step, but I tend to favour explicit
allocation within the test.

Now we can run our tests:

    (require '[clojure.test.check :refer [quick-check]])
    (require '[stateful-check.core :refer [reality-matches-model?]])
    (quick-check 100 (reality-matches-model? queue-spec))

To see more useful test results you can use `print-test-results`:

    (require '[stateful-check.core :refer [print-test-results]])
    (print-test-results queue-spec (quick-check 100 (reality-matches-model? queue-spec)))

## Usage

Using `stateful-check` is a matter of defining a state machine. Each
state machine consists of a map of commands (`:commands`), a function
to generate a command given a state (`:generate-command`), and a
function to clean up after a run (`:cleanup`).

An entry in `:commands` is itself a map specifying the command to be
run. It has the following shape:

    {
     :model/args (fn [state] gen)
     :model/precondition (fn [state args] should-run?)
     :model/next-state (fn [state args result] new-state)
     :next-state (fn [state args result] new-state)
     :real/next-state (fn [state args result] new-state)
     :real/command (fn [& args] (apply run-the-actual-command args))
     :real/postcondition (fn [state args result] passed-test?)
    }

Each of the `model` namespaced keys correspond to functions which are
run during command generation and precondition verification. They
abstractly model how you expect your code to behave.
`:model/next-state` is passed an abstract "result", which will, during
execution, be replaced with the actual return value from the
corresponding execution. At the moment there is no way to perform any
symbolic evaluation on this value.

Each of the `real` namespaced keys correspond to functions which are
run during command execution and postcondition verification. They
track how your code actually behaves, and verify that it corresponds
with your model. The `:real/next-state` function is passed the actual
result of the command's execution, so real results can be stored in
the state.

The `:next-state` function can be used to specify both
`:real/next-state` and `:model/next-state` in situations where the
difference between model results and real results is irrelevant.

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
