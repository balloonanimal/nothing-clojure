(ns nothing
  (:refer-clojure :only [])
  (:require [lambda :refer :all]))

;; basic fns
(def identity (lambda [x] x))
(def constantly (lambda [x] (lambda [_] x)))
;; (def comp (lambda [f] (lambda [g] (lambda [x] (g (f x))))))

;; booleans
(def tru  (lambda [t] (lambda [f] t)))
(def fals (lambda [t] (lambda [f] f)))

;; pairs
(def pair  (lambda [x] (lambda [y] (lambda [z] ((z x) y)))))
(def left  (lambda [p] (p (lambda [x] (lambda [y] x)))))
(def right (lambda [p] (p (lambda [x] (lambda [y] y)))))

;; church numerals
;;; by hand
(def zero  (lambda [f] (lambda [x] x)))
(def one   (lambda [f] (lambda [x] (f x))))
(def two   (lambda [f] (lambda [x] (f (f x)))))
(def three (lambda [f] (lambda [x] (f (f (f x))))))

;;; by successor
(def inc (lambda [n] (lambda [f] (lambda [x] (f ((n f) x))))))

;;; decrementing
;;;; we count up m, keeping track of (m-1, m) when m=n, then m-1=n-1
(def dec
  (lambda [n]
    (left
     ((n (lambda [p] ((pair (right p)) (inc (right p)))))
      ((pair zero) zero)))))
;;;; a difficult to understand definition where we wrap our functions
;; (def dec
;;     (lambda [n]
;;         (lambda [f]
;;             (lambda [x]
;;                 (((n (lambda [g] (lambda [h] (h (g f)))))
;;                   (constantly x))
;;                  identity)))))

;;; arithmetic
(def add  (lambda [n] (lambda [m] (lambda [f] (lambda [x] ((n f) ((m f) x)))))))
(def sub  (lambda [n] (lambda [m] ((m dec) n))))
(def mult (lambda [n] (lambda [m] ((m (add n)) zero))))
(def pow  (lambda [n] (lambda [m] (m n))))

(def four    (inc three))
(def five    ((add two) three))
(def fifteen ((mult five) three))
(def one-hundred ((mult four) ((mult five) five)))

;; modular arithmetic
(def zero? (lambda [n] ((n (constantly fals)) tru)))
(def leq (lambda [n] (lambda [m] (zero? ((minus n) m)))))
(def Z (lambda [f] ((lambda [x] (f (lambda [y] ((x x) y))))
                    (lambda [x] (f (lambda [y] ((x x) y)))))))

(def mod
  (Z (lambda [f] (lambda [n] (lambda [m]
    ((((leq m) n)
      (lambda [x] (((f ((sub n) m)) m) x)))
     n))))))

;; lists
;;; our encoding for lists is to use two pairs
;;; (isnil?, (val, tail))

;; the second arg doesn't matter, identity is the simplest fn
(def empty-list ((pair tru) identity))
(def empty? (lambda [l] (left l)))
(def cons (lambda [h] (lambda [t] ((pair fals) ((pair h) t)))))
(def first (lambda [l] (left (right l))))
(def rest (lambda [l] (right (right l))))

(def reduce
  (Z (lambda [f] (lambda [g] (lambda [n] (lambda [l]
    (((empty? l)
      n)
     (lambda [x] ((((f g) ((g n) (first l))) (rest l)) x)))))))))

(def map
  (Z (lambda [f] (lambda [g] (lambda [l]
    (((empty? l)
      empty-list)
     ((cons (g (first l)))
      (lambda [x] (((f g) (rest l)) x)))))))))

(def range
  (Z (lambda [f] (lambda [a] (lambda [b]
    ((((leq b) a)
      empty-list)
     ((cons a)
      (lambda [x] (((f (inc a)) b) x)))))))))

(def fizzify
  (lambda [n]
    (((zero? ((mod n) fifteen))
      zero)
     (((zero? ((mod n) five))
       one)
      (((zero? ((mod n) three))
        two)
       three)))))

(def fizzbuzz-seq ((map fizzify) ((range one) one-hundred)))
