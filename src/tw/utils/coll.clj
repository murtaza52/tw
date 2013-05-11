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
