;;In this namespace, we broaden our
;;understanding of clojure by examining its
;;plethora of built-in data structures and
;;libraries for working with them.
(ns training.walking)
** Sequences
;;Almost every clojure datastructure that serves as a
;;collection can be viewed through the Sequence abstraction.

;;A sequence is a "lazily" computed stream of values
;;-The source of the sequence is traversed on-demand
;;-Sequences look and act like lists.
;; -Except they are "lazy" vs "Eager".
;; -We only realize portions of the sequence we need
;;-We can work with "infinite" datastructures

;;Basic operations on sequences
;;=============================

;;Range generates a lazy sequence of numbers from 0..bound, exclusive.
(def numbers (range 100))
;;get the first element...
(first numbers)
;;get the nth element
(nth numbers 20)
;;get the last element
(last numbers)
;;return a lazy sequence that draws the first 10 elements
;;and stops
(take 10 numbers)
;;return a lazy sequence that drops the first 10 numbers,
;;starting at 10
(drop 10 numbers)
;;Clump the sequence into pairs
(partition 2 numbers)
;;Return a view of the sequence like a moving window
(partition 2 1 numbers)
;;Generate a sequence using a function
(take 10 (iterate (fn [x] (* x 10)) 1))
;;Concatenate sequences
(concat [:A :B :c] '(:d :e :f))


;;Sequence Comprehensions using =for=
;;===================================
;;We can use the (for [bindings] & body) form to
;;declaratively dissect sequences and return a
;;new lazy sequence
(for [x (range 10)]
  x) 

;;returns the cartesian product of xs and ys as
;;a lazy sequence of vector pairs 
(for [x (range 10)
      y [:a :b :c]]
  [x y])

;;limit the draws to odd values of x using :when
(for [x (range 10)
      y [:a :b :c]
      :when (odd? x)]
  [x y])

;;introduce lexical bindings using :let
(for [x (range 10)
      y [:a :b :c]
      :when (odd? x)
      :let [xx (* x x)]]
  [xx y])

;;traverse sequences using do-seq, good for side-effects
;;Uses the same binding options as for..
(doseq [x (range 10)]
  (println x))

;;Map, Filter, Reduce
;;===================
;;The three musketeers of functional programming.
;;Using these three functions, we can build
;;sophisticated, composeable, computational pipelines without
;;needing to get into goopy loop code.
;;Befriend them....they are invaluable.

;;Map
;;===
;;"Map" a function onto a sequence of values.
;;We can view mapping as a transformation of the
;;input sequence, where the elements of the returned
;;sequence have a function applied to them.
(map (fn [x] (+ x 2)) (range 10))
(map (fn [x] [x x])   (range 10))
;;We can map across multiple sequences simultaneously,
;;and provide a function that takes multiple args..
(map (fn [x y z] (+ x y z)) (range 10) (range 10) (range 10))

;;Filter
;;======
;;Filter takes a predicate function, and a sequence. The resulting
;;sequence only has elements that "passed" the filter, such that
;;when the filter function was applied to the element, the return
;;value is not false.

;;Filter the sequence, retaining only odd numbers.
(filter odd? numbers)
;;Filter evens
(filter even? numbers)

;;Reduce
;;======
;;Reduce allows us to accumulate a result across a
;;sequence, thereby "reducing" the sequence to a single
;;resulting value.  Reduce can be called with or without
;;an initial value for the accumulator.
(defn sum-numbers [xs]
  (reduce + xs))
(sum-numbers numbers)
(defn randomly-sample [xs]
  (reduce (fn [acc x]
            (if (> (rand) 0.5)
              (conj acc x)
              acc)) [] xs))

** Useful built-ins
** Practical Example: FizzBuzz
** Practical Example: Simple statistics
** Maps and Sets

** IO (Input/Output)
** Practical Example: Turtles
** Practical Example: Project Euler
