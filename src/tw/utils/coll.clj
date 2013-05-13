;; This ns contains utility fns for working with collections.
(ns tw.utils.coll
  (:use [midje.sweet :only [fact facts]]))

;; The fn below return a fn which iterates through a seq, testing a pred on each element. It applies the given fn f on the first elem which returns true and updates its value in the seq.
(defn update-first
  [pred f]
  (fn [coll & p]
    (reduce
     (fn [acc x]
       (if (pred x p)
         (reduced (concat (:res acc) [(f x p)] (rest (:coll acc))))
         (assoc acc
           :res (conj (:res acc) x)
           :coll (rest (:coll acc)))))
     {:coll coll :res []}
     coll)))

(def greater-than-30 (update-first (fn [[a b] & _] (> b 30)) (fn [[a b] & _] [a (+ 2 b)])))

(fact "updates the first vector in which the val is greater than 30"
      (greater-than-30 [[:a 20] [:b 30] [:c 50] [:d 60]]) => [[:a 20] [:b 30] [:c 52] [:d 60]])

(defn agg
  "Takes a fn f of a single arg as an argument, and returns a fn which takes a coll and applies f to every elem of the coll and adds the results."
  [f]
  (fn [coll]
    (apply + (map f coll))))

(fact "aggregates and returns the values"
      ((agg :weight) (take 5 (repeatedly (fn [] {:desc "abc" :weight 100})))) => 500)
