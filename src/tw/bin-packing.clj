;; This ns contains generic code to solve a 1D bin-packing problem.
(ns tw.bin-packing
  (:use [tw.utils.coll :only [update-first]]))

(def sample-bin {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50} {:desc :a :weight 30}]})

(def sample-bins [{:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50} {:desc :a :weight 30}]}
                  {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50}]}
                  {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50}]}])

(def sample-item1 {:desc :new :weight 10})

(def sample-item2 {:desc :new :weight 20})

;; fn for retrieving a bin's capacity
(def bin-capacity :capacity)

(bin-capacity sample-bin)

;; fn for retreiving a bin's items
(def bin-items :items)

(bin-items sample-bin)

;; fn for retreiving an item's weight
(def item-wt :weight)

(item-wt sample-item1)

(defn agg
  "Takes a fn f of a single arg as an argument, and returns a fn which takes a coll and applies f to every elem of the coll and adds the results."
  [f]
  (fn [coll]
    (apply + (map f coll))))

((agg item-w) (take 5 (repeatedly (fn [] {:desc "abc" :weight (int (rand 100))}))))

(defn can-add-to-bin?
  [bin-capacity bin-items item-wt]
  (fn [bin [item]]
    ((some-fn pos? zero?) (- (bin-capacity bin)
                             (+ ((agg item-wt) (bin-items bin))
                                (item-wt item))))))

(def can-add-to-sample-bin? (can-add-to-bin? bin-capacity bin-items item-wt))

(defn update-bin
  [bin [item]]
  (assoc bin :items (conj (:items bin) item)))

(update-bin sample-bin [sample-item1])

(bin-capacity sample-bin)

(can-add-to-sample-bin? sample-bin [sample-item1])

(can-add-to-sample-bin? sample-bin [sample-item2])

(defn add-item
  [pred f]
  (update-first pred f))

(def add-sample-items (add-item can-add-to-sample-bin? update-bin))

(add-sample-items sample-bins sample-item1)

(def sample-items [{:desc :new-a :weight 10} {:desc :new-b :weight 20} {:desc :new-c :weight 20}])

(defn add-items
  [pred f]
  (fn [bins items]
    (let [add-f (add-item pred f)]
      (reduce #(add-f %1 %2) bins items))))

(def add-test-items (add-items can-add-to-sample-bin? update-bin))

(add-test-items sample-bins (reverse (sort-by :weight sample-items)))
