;; This namespace contains miscellaneous utilities
(ns tw.utils.misc
  (:use [midje.sweet :only [fact facts]]))

(defn freturn
  "Takes a fn f and a optional return value as input. If f returns a falsy value, then the input return value is returned instead."
  [f x]
  (if-let [v f]
    v
    x))

(fact "returns the given value if the f evals to falsy"
      (freturn 2 3) => 2
      (freturn nil 3) => 3)
