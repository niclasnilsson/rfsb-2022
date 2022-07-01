(ns rfsb.clojure-guide
  "This is a namespace where we add clojure examples.
  Evaluate line by line and see what they return or do."
  {:clj-kondo/config
   {:skip-comments true}}
  (:require
   [clojure.set :as set]
   [clojure.repl :refer [doc source]]
   [clojure.walk :as walk]
   [hiccup.core :refer [html]]
   [lambdaisland.deep-diff2 :as ddiff]))

#_:clj-kondo/ignore


;;;;
;;;; Resources:
;;;;
;;;; https://clojuredocs.org/ - user annotated docs
;;;;


;;;;
;;;; Literal data types
;;;;

;;; Scalars

1                ; integer
3.14             ; double
017              ; octal number (base 8)
0xff             ; hexadecimal number (base 16)
2r1011           ; binary number (base 2)
3r201            ; base 3

7N               ; big int
(bigint 7)       ; big int created from a function

7.2M             ; big decimal
(bigdec 7.2)     ; big decimal created from a function

1/3              ; rational number

(+ 1/3 1/3)      ; math works for rational numbers
(/ 4 5)          ; and integer division often turns into rationals
(/ 4.0 5)        ; you need one of the numbers to be a double if you want the decimal result

(* 3.0 1.01)     ; however, as with most computers and programming languages, decimal math
                 ; is not always correct.  https://en.wikipedia.org/wiki/IEEE_754

(* 3.0M 1.01M)   ; but if you keep everything as big decimals, it works.


;;; strings, symbols and keywords

"Jimi"       ; string
'foo         ; symbol, mostly used for names of things
:name        ; keyword, commonly used as keys in maps
:foo/bar     ; keywords can be namespaced
::bar        ; :: means current namespace

"there are
no special
multiline strings"


"so this string will contain
     more whitespace than you may want"


#"colou?r"   ; regexp

;;; collection literals

[1 2 3]      ; vector
'(1 2 3)     ; list (needs to be quoted when written)
#{1 2 3}     ; set
{:a 1 :b 2}  ; map

[1, 2, 3]    ; commas are always treated as whitespace

(= '12 (quote 12))    ; ' and (quote) is the same thing

; To learn more about literals and the CLojure reader, see
; https://clojure.org/reference/reader

;;;;
;;;; Maps
;;;;

{:a 1 "b" "two"}                  ; keys and vals can be of any type

;;; getting values with get

(get {:a 1} :a)
(get {"a" 1} "a")
(get {"a" 1} "b")                 ; returns nil when not found
(get {"a" 1} "b" 100)             ; can have a default value

;;; getting values using a keyword

(:a {:a 1})                       ; when a keyword is used as a function,
                                  ; it's the same as get})

(comment
  ("a" {"a" 1}))                  ; doesn't work with anything but keywords

;;; more ways to read from a map

(keys {:a 1 :b 2})                ; getting all keys
(vals {:a 1 :b 2})                ; getting all vals

(contains? {:a 1 :b 2} :a)        ; checks is a key is present
(contains? {:a 1 :b 2} :x)

(contains? {:a nil :b 2} :a)      ; and it checks the key, not the value

;;; setting values
;;; (updates the copy, not the original since, everything is immutable)

(assoc {:a 1} :b 2)               ; assoc a value with a key in a map
(assoc {:a 1} :a 2)               ; can "overwrite" as well (the copy, not the original)

;; setting a value in a nested map
(assoc-in {:address {:street "Storgatan 1" :zip 43400}} [:address :zip] 43441)

(dissoc {:a 1 :b 2} :a)            ; removes a key and it's value

;; selecting only certain keys
(select-keys {:a 1 :b 2 :c 3 :d 4} [:b :c])

;; Updating values using a function

(update {:a 1 :b 2} :a inc)        ; apply inc on the value behind :a
(update {:a 1 :b 2} :a + 5)        ; the function can take parameters

(hash-map :a 1 :b 2)               ; can be created from args using hash-map
(into {} [[:a 1] [:b 2]])          ; can be created from pairs using (into {} ...)

(merge {:a 1 :b 2} {:c 3 :d 4})    ; maps can be merged
(merge {:a 1} {:c 3 :d 4} {:e 5})  ; multiple maps as well
(merge {:a 1} [:b 2])              ; you can merge a map and a pair
(merge {:a 1 :b 2} {:a 4})         ; last key wins

(merge nil {:a 1})                 ; you can merge with nil as well
(merge nil nil)                    ; and you can merge nils without explosions

;; map keys are not ordered
{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10}

;; (but array maps are if you really need it. Uncommon, and not something to
;; overuse, but can be useful in some situations)
(array-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10)

;; (and if you mix array-maps with normal maps, keys are again unordered)
(merge (array-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10) {:k 11})

;; reduce-kv allows you to reduce over a map
;; the function takes the map, the key and the value and should return a new map

(reduce-kv (fn [m k v] (assoc m k (inc v))) {} {:a 1 :b 2 :c 3})

;; we have a function in core that can do this also, but reduce-kv is more general:

;; TODO
;; (map-vals inc {:a 1 :b 2 :c 3})

;;;;
;;;; Vectors
;;;;

[1 2 3]                            ; the literal
[:a 4 "hej"]                       ; can contain any values

(vector 1 2 3)                     ; can be created from args using vector

(vec '(1 2 3))                     ; or from another collection using vec
(vec {:a 1 :b 2 :c 3})             ; any kind of collection actually
(vec nil)                          ; and even nil becomes a vector

(assoc [:a :b :c] 1 :x)            ; vectors are associative

(cons 4 [1 2 3])                   ; cons always adds to the head
(conj [1 2 3] 4)                   ; conj adds to the tail on vectors

(comment
  (dissoc [:a :b :c] 1))           ; but dissoc doesn't work

(update [1 2 3] 1 inc)             ; and since it's associative, update works
(update [1 2 3] 1 + 10)

(get [:a :b :c] 1)                 ; ans since it's associatie, get works
(nth [:a :b :c] 1)                 ; but nth is the proper way to get a value at an index


;;;;
;;;; Lists
;;;;

'(1 2 3)                           ; the literal variant
(list 1 2 3)                       ; can be created from args to list

(comment
  (assoc '(:a :b :c) 1 :x))           ; lists are _not_ associative

(get '(:a :b :c) 1)                ; get on lists is _dangerous_, since
                                   ; it always returns nil. Can create some trouble
                                   ; since map, filter etc. often returns lists

(nth '(:a :b :c) 1)                ; nth is the proper way to get a value at an index

(cons 4 '(1 2 3))                  ; cons always adds to the head
(conj '(1 2 3) 4)                  ; conj adds to the head on lists


;;;;
;;;; Sets
;;;;

#{1 2 3}                           ; a set of unique vals, unordered


;; #{1 2 2 3}}                     ; duplicate keys are not allowed

(cons 4 #{1 2 3})                  ; cons adds to the set, but returns a list
(conj #{1 2 3} 4)                  ; conj adds to the set

(comment
  (assoc #{1 2 3} 1 :x))           ; sets are _not_ associative (which makes a lot
                                   ; of sense since they are not ordered)

(#{1 2 3} 2)                       ; sets can be used in function position to see if
                                   ; the argument is part of the set


;;;;
;;;; Collections - things that are common for all or many collections
;;;;

(first [1 2 3])                    ; gets the first value
(first {:a 1 :b 2})                ; gets the first key-value pair
                                   ; (but remember maps are unordered)})

(first nil)                        ; nil are generally safe as a collection
(rest nil)

(rest [1 2 3])                     ; gets the rest of the values
(rest {:a 1 :b 2 :c 3})            ; gets the rest of the pairs

(take 2 [:a :b :c :d])             ; takes the n first
(drop 2 [:a :b :c :d])             ; drops the n first

(drop 2 {:a 1 :b 2 :c 3})          ; works on all kinds collections
(drop 2 #{1 2 3 4 5 6})
(drop 2 nil)                       ; and on nil


;;;;
;;;; Vars
;;;;

(def a 1)                          ; binds a to 1
(def xs [1 2 3])                   ; binds a to a vector
(def square (fn [x] (* x x)))      ; binds square to a function

#'square                           ; reference to the var square (late binding)

;;; vars can have many different characters in their names

(def a 1)
(def a* 1)
(def *a 1)
(def a' 1)
(def _ 1)

(def åäö 1)                        ; unicode characters can however look funny in the REPL
(prn åäö)                          ; but they work fine

(def foo-bar 1)                    ; dashes are fine (and common), since they can't be confsed with minus
(def kebab-case 1)                 ; kebab-case is the common casing in clojure
(def snake_case 1)                 ; not snake_case as in C or Python
(def camelCase 1)                  ; nor camelCase as in C# or Java
(def PascalCase 1)                 ; However, PascalCase is quite common for things that represents
                                   ; structs, entites and such

;;; We often use https://clj-commons.org/camel-snake-kebab/ to convert between different cases


;;;;
;;;; Functions and function calls
;;;;

;;; Basic functions

(defn square [x]                         ; defn is a macro that simplifies binding
  (* x x))                               ; functions to vars

(macroexpand                             ; This is what the macro is doing. You may recogonize
 '(defn square [x]                       ; the result from the code above in the vars section
    (* x x)))


(defn square [x]
  (* x x))


(square 4)                               ; first symbol after the parenthesis is always used as a function

(defn average [x y]
  (/ (+ x y) 2))


(average 4 6)

;; A function always returns the result of the last evaluated expression. But what does
;; a function with no body return?

(defn function-without-body [])

(function-without-body)

;; The reason clojure use prefix notation for math (* 5 (+ 1 2)) instead of infix notation ((1 + 2) * 5)
;; is that +, * and all other operators are just regular functions. Nothing magic.

(* 5 (+ 1 2))

;;; Function documentation

(defn average                            ; functions can have documentation
  "Returns the average of x and y"
  [x y]
  (/ (+ x y) 2))


(average 4.0 6)


(comment
  (doc average)                         ; documentation is metadata on the function and
                                        ; can be read using the doc function in clojure.repl

  (doc +))                              ; all built in functions are of course documented


;;; Function source

(comment
  (source +)                            ; and you can easily see the source of any function
  (source average))                     ; except for the ones just evaluated in the REPL.
                                        ; (the reason is that the source is read from a file)


;;; Variadic function - functions with variable number of arguments

(defn average [& numbers]
  ;; & means "collect the rest of the arguments".  numbers is now a list
  (/ (apply + numbers) (count numbers)))


(average 4 6 8 22 18 2)


(defn average [x y & numbers]           ; you can combine normal args with variadic args
  ;; x and y are regular args, numbers is a list
  (prn "x" x (type x))
  (prn "y" y (type y))
  (prn "numbers" numbers (type numbers)))
  ;; and some clever code to calculate the average

(average 4 6 8 22 18 2)


;;; multi-arity functions - same name, multiple different argument lists

(defn average [& numbers]                   ; the normal function from above
  (/ (apply + numbers) (count numbers)))


(comment
  (average))                                ; doesn't handle 0 arguments well

;; if we would like this function to return 0 when there are no numbers, we can
;; create two argument lists. Note the extra parenthesis around the argument vector
;; and the body.

(defn average
  ([]                                       ; the version with no arguments.
   (average 0))

  ([& numbers]                              ; the version with 1 or more arguments.
   (/ (apply + numbers) (count numbers))))  ; (to be honest, this one match 0 args as we saw above,
                                            ; but clojure finds the exaxt match first. Order doesn't matter.

(average)


;; (defn average                            ; clojure doesn't accept overloads with same arity
;;   ([])
;;   ([]))


;; (defn average                            ; and only one variadic overload is allowed
;;   ([& args])
;;   ([a & args]))


;;; apply a function

;; Sometimes you have a list of arguments, but your function does not accept a list

(def numbers [1 2 3])

(comment
  (+ numbers))                              ; not happy

(apply + numbers)                           ; apply saves the day by
(apply + 10 10 numbers)                     ; apply can actually mix normal args and lists
(comment
  (apply + 10 10 numbers 10))               ; at least if the list is the last argument

(apply + [])                                ; + is safe even if the list is empty and so is *
(apply * [])                                ; so you often don't have to worry about empty lists

(+)                                         ; Same thing without apply of course, but that's
(*)                                         ; a more uncommon situation to find yourself in.

(comment
  (-)                                       ; - and / are a bit more picky though, so they
  (/))                                      ; need some extra care

;;; keywords as functions

(:b {:a 1 :b 2})                            ; keywords can act as functions, which is
(:x {:a 1 :b 2})                            ; a bit clenaer than get

(#{1 2 3} 2)                                ; and so can sets, which is a nice way to see if
(#{1 2 3} 8)                                ; a value exists in the set


;;;;
;;;; "Local variables" (let, letfn)
;;;;



;;;;
;;;; Higher order functions (map, filter, comp, juxt, ...)
;;;;


;;;;
;;;; Thread macros
;;;;

;; sometimes in longer expressions, the normal way of calling functions can be
;; a bit hard to read, since you have to start at the innermost expression and
;; work your way out. This is an example, that is still not too bad (it can be much worse
;; with some more inline functions.

(map str (filter #(zero? (rem % 5)) (map inc (range 1 50))))

;; the same code but on several lined and indented. Doesn't make it clearer at all.
;; also, it's hard to comment out a part of the expression.
(map
  str
  (filter
    #(zero? (rem % 5))
    (map
      inc
      (range 1 50))))

;; Thread macros can help readability.

;; ->> is "thread last" and puts the results of each expression as the _last_ argument
;; of the next expression through the thread
;;
;; Thread last if most often used when handling collections, since most functions operating
;; on collections want the collections in the last position of the call

;; This is now read from left to right, much like pipes on a unix or powershell command line.
(->> (range 1 50) (map inc) (filter #(zero? (rem % 5))) (map str))

;; and complex expressions often becomes much more readable on multiple lines.
;; also, easy to comment out one expression when developing/debugging.
(->>
  (range 1 50)
  (map inc)
  (filter #(zero? (rem % 5)))
  (map str))


;; -> is "thread first" and puts the results of each expression as the _first_ argument
;; of the next expression through the thread
;;
;; Most often used with single values and/or maps

(->
  {:a 1 :b 2 :c 3}
  (assoc :d 4)
  (dissoc :b)
  (merge {:c 60 :e 70}))

;; as-> is the rescue when neither -> or ->> works, since the functions used needs the arguments
;; in different places. This is rather uncommon though, and may sometimes mean that you're trying to
;; do too much at the same time. Think about your options before using as->. The name of the variable
;; can be just about anything, and it's often hard to find a good name. We often use $ instead.

(as->
  (range 1 50) $                       ; last pos
  (filter #(zero? (rem % 5)) $)        ; last pos
  (map vector (range) $)               ; last pos
  (into {} $)                          ; last pos
  (assoc $ 1 1000))                    ; first pos

;; some-> and some->> are quite useful thread macros in some cases. They end the thread is anything
;; becomes nil, and the thread returns nil.

(comment
  (->                                  ; results in a null pointer exception
   {:a 1 :b 2 :c 3}
   (:x)
   (+ 5)))


(some->                                ; returns nil instead
 {:a 1 :b 2 :c 3}
 (:x)
 (+ 5))

;;
;; Lazyness
;;

(def numbers
; not a problem... yet

  (for [x (range 6)]
    (/ 1 x)))


(comment
  (prn numbers)                       ; but problem when materialized


  (def numbers                        ; use doall if you need to materialize eagerly
    (doall
      (for [x (range 6)]
        (/ 1 x)))))


;;
;; clojure.walk
;;


(def data {:a 1 :b {:c 2 :d "foo" :e 3}})

(comment
  (walk/postwalk-demo data)


  (walk/postwalk
    (fn [v]
      (if (int? v) (inc v) v))
    data))


;;
;; diff
;;

(def data {:a 1 :b {:c 2 :d "foo" :e 3}})
(def data' (assoc data :k 4))

(comment
  (ddiff/pretty-print (ddiff/diff data data')))

;;
;; mutability / atoms
;;

(def state (atom {:a 1 :b 2}))

(deref state)
@state ; @ is synonymous to deref (reader macro)

; Use reset! to reset :) the state
(reset! state {:a 1 :b 3})
@state

(reset! state {:a 1})
@state

; Use swap! to apply a function to the current state and replace the state
; with the result of calling that function
(swap! state (fn [state] (assoc state :c 3)))

; swap also accepts a function and arguments if you don't want to inline a function.
(swap! state assoc :c 3)

@state

(swap! state update :c inc)
@state


;;;
;;; Hiccup is a data represenation for HTML
;;;

(defn clock [where hours minutes]
  [:div.clock {:id where}
   [:span.hours hours]
   [:span.minutes minutes]])


(def clocks
  [:div.clocks
   (clock "oslo" 10 00)
   (clock "london" 8 00)])


(html (clock "stockholm" 10 11))
(html clocks)


;;; conditional cond and condp
;;; A macro for writing nested or multiple if:s in a row.

;; cond example
(let [x 6]
  (cond
    (< x 5) "Less than 5"
    (= x 5) "Equal to 5"
    ;(> x 5) "More than 5" ;; syntax to use else cases
    :else "More than 5"))

;; Expands to this
(macroexpand
  '(cond
     (< x 5) "Less than 5"
     (= x 5) "Equal to 5"
     ;(> x 5) "More than 5" ;; syntax to use else cases
     :else "More than 5"))

;; condp example
(let [x 4]
  (condp = x
    1 "One"
    2 "Two"
    3 "Three"
    "More than three"))

;; Expands to this
(macroexpand-1
  '(condp = x
     1 "One"
     2 "Two"
     3 "Three"
     "More than three"))

;; FizzBuzz using condp. Example taken from clojure docs
(defn fizz-buzz [n]
  (condp #(zero? (mod %2 %1)) n
    15 "fizzbuzz"
    3  "fizz"
    5  "buzz"
    n))


(comment
  (into [] (map fizz-buzz) (range 1 20)))
