(ns nothing-with-arity
  (:refer-clojure :only [apply fn])
  (:require [lambda :refer :all]))

;; basic fns
(def identity (fn [x] x))
(def constantly (fn [x] (fn [& _] x)))

;; booleans
(def tru  (fn [t f] t))
(def fals (fn [t f] f))
(def if'  (fn [b t f] (b t f)))

;; pairs
(def pair  (fn [l r] (fn [f] (f l r))))
(def left  (fn [p] (p (fn [l r] l))))
(def right (fn [p] (p (fn [l r] r))))

;; church numerals
;;; by hand
(def zero  (fn [f x]          x))
(def one   (fn [f x]       (f x)))
(def two   (fn [f x]    (f (f x))))
(def three (fn [f x] (f (f (f x)))))

;;; by successor
(def inc (fn [n] (fn [f x] (f (n f x)))))

;;; decrementing
;;;; we count up m, keeping track of (m-1, m) when m=n, then m-1=n-1
(def dec
  (fn [n]
    (left
     (n (fn [p] (pair
                 (right p)
                 (inc (right p))))
        (pair zero zero)))))

;;; arithmetic
(def +    (fn [n m] (fn [f x] (n f (m f x)))))
(def -    (fn [n m] (m dec n)))
(def *    (fn [n m] (m (fn [l] (+ n l)) zero)))
(def pow  (fn [n m] (m n)))

(def four        (inc three))
(def five        (add two three))
(def fifteen     (mult five three))
(def one-hundred (mult four (mult five five)))

;; modular arithmetic
(def zero? (fn [n] (n (constantly fals) tru)))
(def <=    (fn [n m] (zero? (- n m))))

(def Z (fn [f]
         ((fn [x] (x x))
          (fn [x]
            (f (fn [& args]
                 (apply (x x) args)))))))

(def mod
  (Z (fn [f]
       (fn [n m]
         (if' (<= m n)
           (fn [n' m'] ((f (- n m) m) n' m'))
           n)))))

;; lists
;;; our encoding for lists is to use two pairs
;;; (isnil?, (val, tail))

;; the second arg doesn't matter, identity is the simplest fn
(def empty-list (pair tru identity))
(def empty? (fn [l] (left l)))
(def cons (fn [h t] (pair fals (pair h t))))
(def first (fn [l] (left (right l))))
(def rest (fn [l] (right (right l))))

(def reduce
  (Z (fn [f]
       (fn [c n l]
         (if' (empty? l)
           n
           (fn [c' n' l']
             ((f c (c n (first l)) (rest l))
              c' n' l')))))))

(def map
  (Z (fn [f]
       (fn [c l]
         (if' (empty? l)
           empty-list
           (cons (c (first l))
                 (fn [c' l']
                   ((f c (rest l))
                    c' l'))))))))

(def range
  (Z (fn [f]
       (fn [a b]
         (if' (<= b a)
           empty-list
           (cons a
                 (fn [a' b'] ((f (inc a) b) a' b'))))))))

(def fizzify
  (fn [n]
    (if' (zero? (mod n fifteen))
      fifteen
      (if' (zero? (mod n five))
        five
        (if' (zero? (mod n three))
          three
          zero)))))

(def fizzbuzz-seq (map fizzify (range one one-hundred)))
