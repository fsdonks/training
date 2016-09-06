;;In this namespace, we broaden our
;;understanding of clojure by examining its
;;plethora of built-in data structures and
;;libraries for working with them.
(ns training.walking)
;;Common Mathematical Fucntions
;;=============================
(+ 2 3) ;5
(* 2 3) ;6 
(/ 2.0 3.0) ;0.6666666666666666
(/ 2 3) ;2/3 
(Math/pow 2 3); 8.0
(Math/sin Math/PI) 
;1.2246467991473532E-16
(Math/cos Math/PI) ;-1.0 
;;compute remainder using mod
(mod  10 3) ;1 
;;or use quot/rem 
(quot 10 3) ;3  
(rem  10 3)  ;1 
(defn divides-by? [x n]
 (zero? (rem x n)))
(divides-by? 10 2) ;true 
(divides-by? 10 3) ;false

;;Sequences
;;=========
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

;;Range generates a lazy sequence of numbers from [0..bound), exclusive.
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

;;Constructing Sequences and Vectors
;;==================================
;; =conj= is the general mechanism for 
;; conjoining elements to collections
;; - constructs a "new" collection with the conjoined element
;;   - original is preserved (unaltered)
;; - For uniformity, =nil= is treated as the empty-sequence
;; =conj= has different behavior based on the datastructure
;; - lists (eg. seqs) conjoin by prepending items to the head 
;; - vectors conjoin by appending items to the tail
;; =cons= is a sequence-specific alternative to =conj= that 
;; constructs new sequences

;;Construction
;;============
(conj []   2)   ;[2]
(conj [2]  3)   ;[2 3]
(conj '()  2)   ;(2)
(conj '(2) 3)   ;(3 2)
(cons 2 '())    ;(2)
(cons 3 '(2))   ;(3 2)
(cons 4 '(3 2)) ;(4 3 2)
(conj nil 2)    ;(2)
(def xs (list 1 2 3))
(def ys (rest xs))
xs ;(1 2 3)
ys ;(2 3)

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
(let [xs (range 10)
      ys (repeat 2) 
      zs (repeatedly (fn [] (rand-int 100)))]
(map (fn [x y z] (+ x y z))
   xs ys zs))
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

;;Maps and Sets
;;=============
;; Clojure provides implementations of data structures 
;; that represent sorted/unsorted associative maps and sets.
;; - maps associate a /key/ to a /value/
;;   - effectively Sets of [ /key/ /value/ ] pairs.
;;   - no duplicate keys.
;; - sets are analogous to mathematical sets, but store any 
;;   type of clojure data structure
;;   - support union, intersection, and other set-theoretic 
;;     operations  
;;   - no duplicate entries.
;; - Both types are compatible with sequences.
;;Maps 
;;====
;; Because they are so useful and common, Clojure provides a 
;; syntax for them:
;;  - maps are sequences of key / value pairs surround by {} braces.
(def the-map 
  {:first-key "first-value" 
   :second-key "second-value"})
(keys the-map) ;(:first-key :second-key
(vals the-map) ;("first-value" "second-value")
(get the-map :first-key) ;"first-value"
(get the-map :second-key) ;"second-value"
(contains? the-map :third-key) ;false
;;Maps ctd.
;;=========
(def new-map   (assoc the-map :third-key 3))
(def newer-map (dissoc the-map :first-key))
(get new-map   :third-key) ;3 
(get newer-map :third-key) ;nil 
(get newer-map :first-key) ;nil 
(get newer-map :third-key) ;3
(newer-map :third-key) ;3
(seq newer-map) ;([:second-key "second-value"])
(into {} [[:a 0] [:b 1]]) ;{:a 0 :b 1}
(hash-map :a 0 :b 1) ;{:a 0 :b 1}
;;Sets 
;;====
;; Sets also have their own syntax:
;; - sequences of values inside #{} are 
;;   read as sets.
(def s (conj #{} :a))
(contains? s :a) ;true 
(s :a) ;:a 
(filter s [:a :b :a]) ;[:a :a]
(def s2 #{:a :b :c})
(clojure.set/union s s2);#{:c :b :a}
(clojure.set/intersection s s2);#{:a}
(clojure.set/difference s2 s); #{:b :c}
(into #{} [:a :a :a :a :b]) ;#{:a :b}
;;Strings Revisited and Regexes
;;=============================
;; Strings are prevalent in a lot of processing tasks, 
;; so Clojure has a array of functions for dealing with 
;; them.
;; - Of note, regular-expressions are related to 
;;   processing strings.
;; - Regular expressions are delimited by #"..."
;;   - we will not spend time on Regexes unless necessary
;;   - i.e. library usage requires them
(seq "Hi") ;(\H \i) 
(nth "Hello" 3); \l
(clojure.string/replace 
  "Hello" "e" "a") ;"hallo" 
(clojure.string/upper-case 
  "hello") ; "HELLO" 
(clojure.string/lower-case 
 "Hello") ; "hello"

(str "h" \i [:not-a-string])
;"hi[:not-a-string]" 
(clojure.string/join ","
 ["x" "y" "z"]) ;"x,y,z"
(clojure.string/join \newline
 ["x" "y" "z"]) ;"x\ny\nz"
(clojure.string/split 
  "x,y,z" #",") ;["x" "y" "z"]
(defn line->tabs [ln]
 (clojure.string/split ln #"\t"))
(line->tabs 
 (clojure.string/join \tab [1 2 3]))

;;The Road So Far
;;==============
;; We have quite an arsenal of expression by now:
;; - integers, floats, strings, booleans
;; - keywords, quoted symbols
;; - Sequences (lists, vectors, maps, sets)
;; - Functions 
;; We've also seen that Clojure provides a fairly 
;; large standard library for working with these data types.
;; - "Id rather have 100 functions working on a single data type"
;;   - [some hippy]
;; We'll explore a few more useful functions, then move on to 
;; examples
;; - where we can apply what we've seen

;;Useful Builtins
;;===============
(def xs (range 3))
(group-by odd? xs) ;{false [0 2], true [1]}
(group-by
  (fn [x] (if (odd? x) :odd :even))
  xs)   ;{:even [0 2], :odd [1]}
(partition-by
 #(< % 4) (range 5)) ;((0 1 2 3) (4))
(map-indexed (fn [idx x] [idx x]) xs)
;;([0 0] [1 1] [2 2])
(def m {:a {:b 1}})
(assoc-in m [:a :c] 2)  ;{:a {:b 1 :c 2}}
(update-in m [:a :b] inc);{:a {:b 2}}
(update-in m [:a :b] + 2) ;{:a {:b 3}}
(get-in m [:a :b]) ;1
(vec (range 2)) ; [0 1]
(vec #{:a :b})  ; [:b :a]
(into [] #{:a :b}; [:b :a]
(into [] (map inc) (range 2)) ;[1 2]
(into [:z]  {:a 2 :b 3}) ;[:z [:a 2] [:b 3]]

** IO (Input/Output)
** Practical Example: Turtles
** Practical Example: Project Euler
