(ns rfsb.excercise-collections)

(def data {:a 1 :b 2 :c 3})

(def xs [{:a 1 :b 3}
         {:a 2 :b 1}
         {:a 3 :b 7}
         {:a 4 :b 6}
         {:a 5 :b 6}])

(def ys (range 5 20))



;;
;; Excercise 1: map-vals
;;

;; In order to update all values in a map, we can use reduce-kv.
(reduce-kv (fn [m k v] (assoc m k (inc v))) {} data)


;; However, the code is not very readable since reduce-kv is
;; very multi-purpose, and we'd rather have a (reusable)
;; function that just maps the vals. Implement a function
;; that can be called like this. You can use any function
;; in clojure.core to inside your function.


(defn map-vals [m f])
  ;; TODO: implement this


(map-vals data inc)             ;; {:a 2 :b 3 :c 4}
(map-vals data str)             ;; {:a "1" :b "2" :c "3"}


;;
;; Exercise 2: filter
;;
;; a) Filter out all xs where a is odd
;; b) Filter out all xs where the sum of a and b are odd
;; c) Filter out all xs where the sum of a and b is 10.
;;    Use an anonymous function.
;; d) Filter out all xs where the sum of a and b is 10.
;;    Don't use an anonymous function.
;; e) Filter out all xs where the sum of a and b is 10.
;;    Reuse your function from d) somehow.


;;
;; Exercise 3: map
;;
;; a) Transform xs until they look like this:
;;    [4 3 10 10 11]


;; b) Transform xs until they look like this:
;;    [{:sum 4}
;;     {:sum 3}
;;     ,,,]

;; c) Return the sum of all a:s and b:s in xs.



;;
;; Exercise 4: reduce
;;

;; a) Return the sum of all a:s and b:s in xs, using reduce.


;; b) Given xs, return a sequence (vector, list, ...)
;;    that contains maps with only the key/value of the
;;    highest value.
;;
;;    [{:b 3}
;;     {:a 2}
;;     {:b 7}
;;     {:b 6}
;;     {:b 6})

;; c) Given xs, create a function that returns the first
;;    map found containing an a higher than y.
;;
;; d) If always traversed the whole coll in your c) 
;;    solution, find a way to exit reduce early. Look in the
;;    stdlib docs for useful things.

(defn first-higher-than [y coll])


(first-higher-than 2 xs)




