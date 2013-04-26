(ns tw.tree
  (:use [clojure.zip :only [seq-zip end? root node up down] :as zip]))

(def tr [+ 1 2 3 [- 4 3] 5 6])

(defn print-tree [original]
  (loop [loc (zip/seq-zip (seq original))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
                (do (println (zip/node loc))
                    loc))))))



(defn factorial3 [x]
  ((fn [x y]
      (if (= x 0)
          y
          (recur (- x 1) (* x y)))) x 1))

(factorial3 5)

(print-tree tr)


(defn add [x y]
  (+ x y))

(defn oppos [a]
  (loop [n 0]
    (if (fn? (nth a n))
      n
      (if (< (+ 1 n) (count a))
        (recur (inc n))
        -1 ))))

(defn replaceleaf [a leaf opp]
  (concat (take (- opp 3) a)
          [leaf]
          (take-last (- (count a) opp) a)))

(defn rpncalc [exp]
  (let [opp (oppos exp) leaf ((nth exp opp) (nth exp (- opp 2)) (nth exp (- opp 1))) ]
    (if (= (count exp) 3) leaf
        (rpncalc (replaceleaf exp leaf (+ opp 1))))))

(rpncalc [+ 1 2 3 [- 5 4] 3 4])
