;; This ns contains fns for working with collections.
(ns tw.utils.coll)

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

(greater-than-30 [[:a 20] [:b 30] [:c 50] [:d 60]])

(item-wt sample-item1)

(defn agg
  "Takes a fn f of a single arg as an argument, and returns a fn which takes a coll and applies f to every elem of the coll and adds the results."
  [f]
  (fn [coll]
    (apply + (map f coll))))

((agg item-w) (take 5 (repeatedly (fn [] {:desc "abc" :weight (int (rand 100))}))))
