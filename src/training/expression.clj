;;A simple exposition of how to express
;;things in clojure.
(ns training.expression)
;;We'll approach clojure from the view of
;;linguistics, rather than other approaches.
;;While there will be plenty of discourses
;;on mathematical topics, I find it easier to
;;view clojure as a language, and to explore
;;how we - as humans - can express things in
;;this language.  I think you'll be pleased
;;to find that Clojure provides an interesting
;;balance of depth and power:
;;It takes minimal effort to become "dangerous",
;;or fluent in expressing complicated topics
;;in clojure, but the outward simplicity belies
;;the expressive depth clojure provides.  In other
;;words, clojure grows with you....baby steps are
;;easy and undaunting, but you - like many others
;;- may be surprised at the expressive power that
;;such a relatively "simple" language provides.


;;Primitive Expressions
;;=====================
;;Clojure is a language, just like English or
;;Spanish, or Chinese, or Klingon.  The only
;;difference between aforementioned languages
;;and languages like clojure is the intent:
;;clojure is intended to serve primarily
;;as a medium for expressing computations
;;to a computer, in a human-readable format.

;;Due to some unique characteristics in clojure's
;;pedigree, and the conscience design choices of
;;the core development team, the language serves
;;for more purposes than "merely" describing computations.
;;Or, from another angle, "describing computations" in
;;clojure carries far more utility than in other languages
;;aimed at the same purpose.

;;The REPL - your conversational partner
;;======================================

;;When we work "in" clojure we're typically working
;;"in" the Read Evaluate Print Loop (REPL), or
;;writing chunks of text that are ultimately
;;destined for the REPL.

;;I find it helpful to think of the REPL as an
;;active listener.  It is simultaneously:
;;-a partner that can read and write clojure as its native tongue
;;-waits for you to say something
;;-always responds to what you say
;;-and waits for the next piece of the conversation.

;;In other words, the REPL continually
;;1. =R=eads input (typically from you, the user),
;;2. =E=valuates the input,
;;3. =P=rints output,
;;4. =L=oops back to 1., that is read the next input

;;The REPL is your gateway to expressing computations
;;and other things in clojure.  Once you get used to
;;conversing with it, you will find the REPL is an
;;invaluable tool, and that it lends a "liveliness" to
;;the experience, so that "programming" feels more like
;;conversing.

;;The REPL Comprehends Clojure
;;============================
;;How then, do we converse with our partner?
;;Assuming you have an active REPL, you will see
;;a prompt, akin to:
;;user>
;;This prompt is the REPL "waiting" for you to tell it
;;something.
;;When we communicate with the repl, we enter in
;;some characters via the keyboard, and then (typically)
;;hit the "enter" key.

;;Evaluate the following expression:
"hello REPL"
;;should yield:

;;"hello REPL"
;;user>

;;You sent some input for the REPL to Read and Evaluate,
;;it Printed the result, then continued Reading input
;;by presenting you with the prompt.

;;It turns out the input you sent "hello REPL" was
;;something the REPL comprehended, and it merely printed
;;the input back at you.  Let's delve into that...

;;Primitive Expressions
;;=====================
;;Like any language, clojure has the notion of "syntax", that
;;is rules that govern how sentences are built, what valid
;;sentences look like, etc.  In English, we have letters,
;;that is A-Z,a-z, as well as punctuation, like the humble period . ,
;;and numbers 0-9.  There is also the notion of "whitespace", that
;;is symbols that denote the separation of words.  An English sentence
;;may look like:
;;-This is a simple sentence.

;;When you read the preceding sentence, your brain automatically applied
;;the English syntax to figure out that:
;;1. There is a sentence, because we have a period.
;;2. Since sentences are composed of words
;;   - There are words in the stuff before the period.
;;   - Words are separated by spaces, so we think there are
;;     5 words.
;;Your brain is acting just like a REPL, but
;;for the English language.  It Reads the input, Evaluates the
;;sentence, teasing out the structure and meanin, and does something
;;with the result.  If you spoke the sentence out loud, we could view
;;that as analogous to Printing the result.

;;We can think of letters as the most primitive form of communication
;;in English.  That is, when our brain-REPL tears apart a sentence,
;;it never tries to tear apart letters (or numbers).  In fact, we
;;leave them alone.....in other words, Evaluating a primitive returns
;;the primitive (there's nothing to do).  Evaluating a sentence
;;may return less-primitive things, like a collection of words,
;;or if our rules are sophisticated, a subject and a predicate,
;;phrases, etc.

;;Clojure Primitives
;;==================
;;If primitives evaluate to themselves, then they form a building
;;block for the language.  We can view the language as a system of
;;rules used to project meaning onto combinations of primitive constructs.
;;Primitives, then, may be seen as a form of data.  The language defines
;;how data may be arranged (syntax) and interpreted to have meaning (semantics).

;;Clojure has several primitive types that, when entered into the
;;REPL, evaluate to themselves.  We treat these as data.  More importantly,
;;since they are primitive, neither we (nor Clojure) requires any
;;additional information to explain what they mean or how to evaluate
;;them.

;;Characters
;;==========
;;Characters are like individual letters or numbers in English,
;;with the addition of a leading back-slash to indicate the
;;data is a character and not a symbol.
\a ;the character "a" 
\b ;the character "b"

;;Strings
;;=======
;;Strings are collections of characters, indicated by surrounding one
;;or more characters with double-quotes:
"We have already seen strings."
;;We'll dissect the following line in the next section, but for
;;now, evaluate it to see what happens:
(seq "I contain characters")
;;Should yield
;(\I \space \c \o \n \t \a \i \n \space \c \h \a \r \a \c \t \e \r \s)

;;To foreshadow, we told the REPL to evaluate an expression,
;;one which applied 'seq' to the string, and the REPL dutifully
;;printed the result.  Mentally bookmark that example as we
;;move on.

;;Numbers
;;=======
;;Numbers are fairly consistent with our idea of what numbers should
;;look like.  Clojure comes from a long line of mathematically sophisticated
;;languages, so the REPL understands many kinds and formats of numbers, and
;;knows how to handle them gracefully.  We will explore common numbers.

;;Integers
;;========
1
2
3
100000000000

;;Floats
;;======
;;Floats are denoated by the decimal point representation:
1000.2

42.42
;;Ratios
;;======
;;Clojure provides facilities for dealing with ratios explicitly.
;;Unless otherwise instructed, division will always produce a ratio
;;if even division cannot be accomplished.  If we want to, we
;;express ratios directly as a forward-slash, flanked by
;;integers:

42/5
33/2

;;Booleans
;;========
;;Clojure understands primtive notions for true and false, also known
;;as Boolean values - named after George Boole.
true
false

;;nil
;;===

;;The symbol nil has some unique properties, one of which is
;;primitive evaluation.
nil
;;yields nil.

;;=nil= is equivalent to false with respect to logical truth,
;;however, it has other uses.  =nil= is a common return result
;;from the REPL, and is typically used to indicate emptiness or
;;falseness.

;;Exotic Primitives
;;===========
;;Clojure provides many other constructs that have primitive evaluation
;;rules.  Here are two of the most important....


;;Keywords
;;========
;;Keywords are a common idiom in clojure expressions, and are formed by
;;prefixing a colon, : , onto a
;;- any combination of characters

:a
:b
:this-is-a-keyword
:THISisALSOaKEYWORD

;;Quoted Symbols
;;==============
;;Any input prefaced by the single-quote, ', is treated as primitive
;;data.  That is, the REPL evaluates it to be identical to the input.
;;Much like in English, this process is called "quoting".  We're literally
;;"quoting the source", that is telling the REPL to treat the quotation
;;exactly as it came:
'2
'"three"

;;Symbols are unadorned collections of characters that
;; - may not be begin with a number
;;Quoted symbols return data like anything else...
'x
'this-will-return-a-symbol
;;The result printed back is identical to the input, with
;;the notable exception of the original quotation mark.
;;Clojure interpreted the data, 'x, as being a meaningless
;;symbol that just happened to look like "x".  There is,
;;however, a difference between x and "x" and \x....

;;Symbols play a crucial role in giving meaning to expressions, and
;;they have non-primitive evaluation rules. 
;;What happens if we don't use a quote?  Foreshadowing....
;;We'll get into trouble because clojure will NOT interpret the
;;thing we're evaluating as data.....this requires us to give meaning
;;to the symbol in order for Clojure to evaluate it.
x
;;Clojure prints an error if we try to evaluate an unquoted symbol:
CompilerException java.lang.RuntimeException:
Unable to resolve symbol: x in this context,
compiling:(C:\Users\tspoon\AppData\Local\Temp\form-init8366984737339229996.clj:1:5500)
;;The error tells us what the problem is (x is undefined in this context), and
;;where is occurred.

;;In the next section we'll see how to express complex ideas, like defining what
;;x means, all while conversing with the REPL.

;;Complex Expressions
;;===================
;;In the original English example, we used a sentence to gain familiarity
;;with expressing a complicated concept from primtive concepts.
;;Letters formed words, words formed a phrase, and a phrase ending with a
;;period formed a sentence.  In each of the preceding examples, we built
;;something complex out of something primitive.

;;Letters => Words
;;Words => Phrase
;;Phrases + \. => Sentence

;;Interestingly enough, we were able to decompose a complex setence into
;;its component pieces via the same rules:

;;Sentence => Phrases + \.
;;Phrase => Words
;;Word => Letters

;;In this case, letters are the most primitive elements of a sentence.
;;Since we have defined Clojure primitives, are there analagous ways to
;;compose them into things like phrases, and sentences?

;;Lists
;;=====
;;Lists are the fundamental means of composing primitives into complex
;;expressions.  In fact, they are central to expressing anything in Clojure.
;;Lists are denoted by enclosing pairs of parenthesis.  For now, we will
;;quote them to treat them as data.:
'(this is a quoted list that will not be evaluated)
;;We can put anything inside the list, including other lists:
'(this list contains (another list) )
'(1 first 2 second 3 third)
'("Lists" :can 'contain :different "types" (:like-numbers 0 1 2 3 4 5 6 7))

;;Unquoted Lists Are Complex Expressions
;;======================================
;;If we remove the preceding quotation mark, the list is no longer
;;data - per our primitive evaluation rules.  What will the REPL do?

;;Clojure follows an interesting evaluation rule regarding lists:
;;- the first element of the list is treated specially.
;;  - the REPL will =E=valuate this element and then =A=pply the
;;    result to the remaining elements
;;- Any elements of the list after the first element are treated
;;  as arguments.  They are evaluated, and their results are used
;;  as input to Apply.
;;Let's try simple arithmetic first.
;;Assume, for the sake of argument, that in the REPL's universe,
;;the symbol '+ is pre-defined as addition.
(+ 2 3) 
;;Yields 5

;;Assume we have similar operators associated with the symbols '*,
;;'/, for multiplication and division respectively...
(* 2 3)
;;yields 6
(/ 2 3)
;;yields 2/3 


;;What happens if we insert an arbitrary symbol into the beginning
;;of the list?
(f 2 3)
;;We get an error because f is not defined...
;;CompilerException java.lang.RuntimeException:
;;Unable to resolve symbol: f in this context,
;;Compiling:(C:\Users\tspoon\AppData\Local\Temp\form-init8366984737339229996.clj:1:1) 

;;How can we define 'f? Or any other symbol?

;;In clojure parlance, the structure (f x1 x2 x3 ....) is called a
;;=form=, with the first element of the list denoting what kind of
;;form it is.  Clojure has some useful built-in forms that
;;define special rules for evaluation.  These "special forms" or
;;built-in forms are the basis of clojure.  The first special form
;;is (def name expr), which lets us define things.

;;Any time the REPL is passed a form, i.e.
;;(something x y z and more args ....)
;;the REPL will attempt to evaluate the form and print the result.

;;Invalid forms will trigger exceptions.

;;Using def and symbols to define Vars
;;=====================================
;;Clojure provides many built-in symbols that allow
;;us to express things.  Key amongst this is =def=,
;;which allows us to define a symbol, to give it meaning.
(def x 2)
;;yields
;;nil
;;user>

;;In this case, the REPL has acknowledged our request
;;to define x as the integer 2, and printed nil in
;;response.  nil is a typical result for operations
;;like def, which cause a side-effect, but yield no
;;useful value.

;;Now, if we evaluate x, unquoted.....
x
;;yields
;;2

;;user>
;;Vars
;;====
;;Specifically, the symbol x has meaning in the
;;current context.  The current context includes
;;a place to hold defined symbols like =x=, called
;;a =namespace=.  So, in the current namespace,
;;=training.expression=, the REPL can now resolve
;;the meaning of =x=.  In Clojure parlance, we say
;;that =x= is "bound" to the value 2.  This binding
;;implies that =x= is a Var, or defined symbol,
;;which resides in the training.expression namespace.

;;From this point forward, when we evaluate bound symbols,
;;that is Vars, the resulting evaluationg will be the
;;value passed in during def.

(def person "Tom")
;;=person= is bound to "Tom"
(def color :blue)
;;=color= is bound to :blue

;;We can refer to specific vars using the
;;long-form prefixing the namespace and a forward-slash
;;to the var: 
training.expression/person
;;evaluates to "Tom" 

;;Clojure has a pred-defined var called =list=
;;the allows us to construct lists.
(def the-list (list person color))
;;When the REPL evaluated the preceding expression,
;;it first evaluated 'def, resolving it into
;;clojure's built-in definition facility.
;;According to the rules of def, clojure knows
;;the that second symbol is supposed to be the
;;thing being defined, and so the REPL does not evaluate
;;=the-list=.
;;Finally, the REPL evaluates the definition of =the-list=,
;;the third argument of the application of (def ...).

;;This is another list, which means evaluation starts again...
;;  -With =list= as the thing to apply
;;  -and =person=, =color= as arguments...
;;     -=list= evaluates to the built-in facility for creating lists.
;;     -=person= evaluates to the string "Tom", per our definition.
;;     -=color= evaluates to the keyword :blue, per our definition.
;;  -(list "Tom" :blue) evaluates to '("Tom" :blue)

;;The var =the-list= refers to the resulting '("Tom" :blue) list.
;;We can use other built-in definitions to examine the =the-list=
(first the-list)
;;"Tom" 
(second the-list)
;;:blue
(rest the-list)
;;(:blue) 

;;Collecting Data With Vectors
;;============================
;;Rather than using quoted lists, we can accomplish
;;a similar function - denoted a collection of
;;data - using a clojure vector.
;;Vectors have special syntax, denoted by wrapped
;;brackets, [...], and may also be constructed
;;using the =vector= symbol.
(def v1 ["this" :is :a 'vector])
v1 
;;["this" :is :a 'vector]
(def v2 (vector :also :a :vector))
v2
;;[:also :a :vector]

;;Our positional functions work on vectors as well...
(first v2)
;;:also
(second v2)
;;:a
(rest v2)
;;(:a :vector)

;;Note that evaluating =rest= returned what appears to be a
;;list, not a vector.  We'll discuss this behavior when
;;we work with sequences later.

;;Vectors are idiomatic in clojure, since they specifically
;;denote data, and they have some extremely useful performance
;;characteristics.  From a lingustic perspective, vectors
;;give us a visual cue that we're working with a data structure,
;;which, unlike a like list, will not be interpreted as a
;;form to be evaluated.

;;Clojure's built-in functions expect "bindings", that is
;;associations between symbols and values, to be expressed
;;as vectors, which is the impetus for introducing them now.
;;With vectors in hand, we can use more built-in functionality to
;;evaluate more complex expressions...

;;Local definitions with =let=
;;============================
;;Clojure provides another means for binding symbols to
;;values.  When we look at the behavior of =def=, we see
;;the that definition is in a sense, global.  If =x= is
;;defined to be a value, the var =x= is bound to said value
;;everywhere we evaluate =x=, unless something changes (
;;maybe redefine =x=...).

;;Rather than rely on global bindings, we introduce a
;;more localized, ad-hoc form of binding vars.  The clojure
;;=let= form (let [& bindings] body) defines local, =lexically-scoped=
;;Vars that hold in the expression contained by let.

;;When the REPL sees a =let= form, it expects a =vector= of
;;bindings, which act much like our bindings from the =def= form,
;;followed by a /body/ to evaluate.  =let= effectively creates a
;;pocket environment, where the symbols are bound according to
;;the supplied bindings, then in this environment, the /body/
;;is evaluated.  Outside of the let form, the bindings do not exist.
;;They are said to be lexically scoped to the =body= of the let.

;;For example,
(let [state "Texas"]
  state)
;;yields
;;"Texas"
;;Despite the fact that we never defined =state=, inside the
;;=let= form, state is defined and bound, so the REPL can
;;evaluate it.  

;;We can have multiple bindings for a let: 
(let [state "Texas"
      city "San Antonio"]
  [city state])
;;["San Antonio" "Texas"]

;;We can also nest let forms, like any other expression:
(let [state "Texas"]
  (let [city "San Antonio"]
    [city state]))
;;["San Antonio" "Texas"]

;;Interstingly, we can bind existing Var definitions, and
;;temporarily override, or =shadow= them inside of a let:
(let [state "Texas"]
  (let [state "Virginia"]
    state))
;;"Virginia"









