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
        (swap! queue pop)))

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
    (require '[stateful-check.core :refer [reality-matches-model? print-test-results]])
    (quick-check 100 (reality-matches-model? queue-spec))
    ;=> {:result false, :seed 1417008254272, :failing-size 5, :num-tests 6, :fail [[[#<0> (:create)] [#<1> (:push #<0> 3)] [#<2> (:pop #<0>)]]], :shrunk {:total-nodes-visited 2, :depth 0, :result false, :smallest [[[#<0> (:create)] [#<1> (:push #<0> 3)] [#<2> (:pop #<0>)]]]}}

Whoops! It looks like we've got a bug! Let's have a look at the failing case:

    (print-test-results queue-spec (quick-check 100 (reality-matches-model? queue-spec)))

This should give us some output like this:

    Failing test case:
       #<0> = (:create) 	;=> #<Atom@d3a4ac1: #<PersistentQueue clojure.lang.PersistentQueue@1>>
       #<1> = (:push #<0> 1) 	;=> nil
       #<2> = (:pop #<0>) 	;=> #<PersistentQueue clojure.lang.PersistentQueue@1>
       !! Postcondition failed !!
    Shrunk:
       #<0> = (:create) 	;=> #<Atom@6e01cbaf: #<PersistentQueue clojure.lang.PersistentQueue@1>>
       #<1> = (:push #<0> 1) 	;=> nil
       #<2> = (:pop #<0>) 	;=> #<PersistentQueue clojure.lang.PersistentQueue@1>
       !! Postcondition failed !!

So, we have an error when we create a queue, then push a value into
it, then pop the value back out. That could be an issue with
`push-queue` or with `pop-queue`.

Looking at the return value of the `:pop` instruction, though, we can
see that it's returning the wrong thing! It should be retuning the
popped value, but it's returning the entire queue. We forgot to put
`val` in there. That's easy to fix:

    (defn pop-queue [queue]
      (let [val (peek @queue)]
        (swap! queue pop)
        val))

There we go now. How about trying the test again now:

    (quick-check 100 (reality-matches-model? queue-spec))
    ;=> {:result true, :num-tests 100, :seed 1417008522208}

We're passing now. Hooray!

## Specification format

Using `stateful-check` is a matter of defining a state machine. Each
state machine consists of a map of commands (`:commands`) and a
function to generate a command given a state (`:generate-command`).

### `:commands`

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

### `:generate-command`

The `:generate-command` function must be a function from the abstract
state to a generator for a valid command.

### `:setup`/`:cleanup`

You can also provide a `:setup` and/or a `:cleanup` function.

The `:setup` function must be a function of zero arguments, and its
return value is used as the initial `state` for command execution (not
generation). It will be called prior to a run of generated commands.

The `:cleanup` function must be a function of one argument: the real
state. Its return value is ignored. It will be called after a run of
generated commands.


## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
