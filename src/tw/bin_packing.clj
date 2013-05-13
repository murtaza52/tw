;; This ns contains generic code to solve a 1D bin-packing problem. The genric fns defined in here can be then used with any type of bin-packing problem, such as adding talks to tracks/sessions.
(ns tw.bin-packing
  (:use [tw.utils.coll :only [update-first agg]]
        [midje.sweet :only [fact facts]]))


;; the bin packing algorithm
;; ####################################################################

;; algorithm for detrming if an item can be added to the bin or not.
;; the below fn takes the following three fns as input and returns a fn-
;; 1. bin-capacity - a fn which determines the capacity of a given bin.
;; 2. bin-items - a fn which returns a seq of items in a given bin.
;; 3. item-w - a fn which returns the wt of a given item.
;;
;; the returned fn takes two inputs - a bin and an item and returns true/false if the item can be added to the bin.
(defn can-add-to-bin?
  [bin-capacity bin-items item-w]
  (fn [bin [item]]
    ((some-fn pos? zero?) (- (bin-capacity bin)
                             (+ ((agg item-w) (bin-items bin))
                                (item-w item))))))

;; fn for adding an item to the bin. This fn is used by the below algorithm.
(defn add-item
  [pred f]
  (update-first pred f))

;; returns a fn for adding items given a seq of bins and items.
;; The fn below takes three fns as inputs -
;; 1. pred to decide if an item can be added to the bin.
;; 2. fn f for adding the item to the bin.
;; 3. fn fsort to sort the items.
(defn add-items
  [pred f fsort]
  (fn [bins items]
    (let [add-f (add-item pred f) sorted-items (reverse (fsort items))]
      (reduce #(add-f %1 %2) bins sorted-items))))


;; ##############################################################################################
;; sample data and fns to test the bin-packing algorithm.
(def sample-bin {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50} {:desc :a :weight 30}]})

(def sample-bins [{:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50} {:desc :a :weight 30}]}
                  {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50}]}
                  {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50}]}])

(def sample-item1 {:desc :new :weight 10})

(def sample-item2 {:desc :new :weight 20})

;; fn for retrieving a bin's capacity
(def bin-capacity :capacity)

(fact "returns the bin's capacity"
      (bin-capacity sample-bin) => 100)

;; fn for retreiving a bin's items
(def bin-items :items)

(fact "returns the bin's items"
      (bin-items sample-bin) => [{:desc :a, :weight 10} {:desc :a, :weight 50} {:desc :a, :weight 30}])

;; fn for retreiving an item's weight
(def item-wt :weight)

(fact "returns the wt of the item"
      (item-wt sample-item1) => 10)

(def can-add-to-sample-bin? (can-add-to-bin? bin-capacity bin-items item-wt))

(facts "returns true if the item can be added to the bin"
       (can-add-to-sample-bin? sample-bin [sample-item1]) => true
       (can-add-to-sample-bin? sample-bin [sample-item2]) => false)

;; fn for adding the item to the bin
(defn update-bin
  [bin [item]]
  (assoc bin :items (conj (:items bin) item)))

(fact "adds the item to bin"
      (update-bin sample-bin [sample-item1]) => {:items
                                                 [{:desc :a, :weight 10}
                                                  {:desc :a, :weight 50}
                                                  {:desc :a, :weight 30}
                                                  {:desc :new, :weight 10}],
                                                 :capacity 100})

(fact "returns the bin-capacity"
      (bin-capacity sample-bin) => 100)

(def sample-items [{:desc :new-a :weight 10} {:desc :new-b :weight 20} {:desc :new-c :weight 20}])

(def add-test-items (add-items can-add-to-sample-bin? update-bin #(sort-by :weight %)))

(fact "adds the items to given bins"
      (add-test-items sample-bins sample-items) =>
      '({:items
         [{:desc :a, :weight 10}
          {:desc :a, :weight 50}
          {:desc :a, :weight 30}
          {:desc :new-a, :weight 10}],
         :capacity 100}
        {:items
         [{:desc :a, :weight 10}
          {:desc :a, :weight 50}
          {:desc :new-c, :weight 20}
          {:desc :new-b, :weight 20}],
         :capacity 100}
        {:items [{:desc :a, :weight 10} {:desc :a, :weight 50}],
         :capacity 100}))
