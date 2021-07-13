(ns conversion
  (:require [nothing :as n]))

(defn cbool->bool [cbool] ((cbool true) false))

(defn cint->int [n] ((n inc) 0))
(defn int->cint [n] (nth (iterate n/inc n/zero) n))

(defn list->clist [list]
  (reduce (fn [tail head] ((n/cons head) tail)) n/empty-list (reverse list)))

(defn clist->list [clist]
        (loop [clist clist
               list []]
          (if (cbool->bool (n/empty? clist))
            list
            (recur (n/rest clist) (conj list (n/first clist))))))
