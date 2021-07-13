(ns lambda)

(defmacro lambda [var-bindings body]
  (assert (vector? var-bindings))
  (assert (every? symbol? var-bindings))
  `(fn [~@var-bindings] ~body))

(defmacro new-lambda [var-binding body]
  (assert (vector? var-binding))
  (assert (= 1 (count var-binding)))
  (assert (every? symbol? var-binding))
  `['~'x '~body])
