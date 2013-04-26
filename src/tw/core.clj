(ns tw.core
  (:use [midje.sweet :only [fact facts truthy falsey]]
        [clojure.string :only [split]]
        [clojure.algo.generic.functor :only [fmap]]
        [clojure.set :only [difference]]
        [swiss-arrows.core :only [-< -<>]]
        [clojure.pprint :only [pprint]]))

(def text1 (slurp "src/tw/input2.txt"))
;; (def text "Ruby Errors from Mismatched Gem Versions 45min
;; Common Ruby Errors 45min")

(def text2 (slurp "src/tw/input.txt"))

(def split-by-nline #(split % #"\n"))

(fact "Splits lines"
      (split-by-nline text2) => ["Ruby Errors from Mismatched Gem Versions 45min" "Common Ruby Errors 45min"])

(def split-by-tab #(split % #"\t"))

(fact "splits by tabs"
      (split-by-tab "a\tb") => ["a" "b"])

(def split-by-space #(split % #" "))

(fact "splits by spaces"
      (split-by-space "a b c") => ["a" "b" "c"])

;; Returns the coll without the last element
(def rest-last #(-> % reverse rest reverse))

(fact "returns a coll without the last element"
      (rest-last [1 2 3 4 5]) => [1 2 3 4])

(defn freturn
  [f ret]
  (if-let [res f]
    res
    ret))

(defn extract-int-string
  [str]
  (freturn (->> str (re-matches (re-pattern "(\\d+).*")) second) str))

(fact "returns the int part of the string"
      (extract-int-string "24min") => "24")

(freturn (extract-int-string "m24min") "lightning")

;;todo -see how to parse diff patterns

(defn str->int
  [str]
  (freturn (if (re-matches (re-pattern "\\d+") str) (read-string str))
           str))

(fact "parses the string to an int"
      (str->int "25") => 25
      (str->int "gh4") => nil)

(def words-to-mins-hash {"lightning" 5})

(def words-to-mins #(freturn (words-to-mins-hash %) %))

(fact "returns mins for given string"
      (words-to-mins "lightning") => 5
      (words-to-mins 20) => 20)

(defn get-data
  [text]
  (let [r (-< (split-by-nline text)
              (->> (map split-by-space) (map rest-last) (map #(interpose " " %)) (map #(apply str %)))
              (->> (map split-by-space) (map last) (map extract-int-string) (map str->int) (map words-to-mins)))]
    (map #(vector %1 %2) (first r) (second r))))


(get-data text2)

(pprint (get-data text2))

(defn get-total
  [coll]
  (reduce #(+ %1 (second %2)) 0 coll))

(def track-def [[9 12] [1 5]])

(def weighted-num #(/ % 60))

(def weighted-total #(-> % get-total weighted-num double))

(defn talk-exists?
  [talk tracks]
  (not-any? #(= % track)))

(weighted-total [[:a 30] [:b 60]])

(def empty-track [[] []])

(def add-track (partial cons empty-track))

(defn assemble-tracks
  [talks]
  (loop [[f & r] talks tracks empty-track]
    (cond
     (nil? f) tracks
     :else (recur r (add-to-track f tracks)))))

(defn add-to-track
  [talk tracks]
  ()
  (cond
   ())
  (for [track tracks]))

(assemble-tracks d)

(def conversions (slurp "src/tw/conversions.txt"))

(->> conversions split-by-nline (map split-by-tab) (into {}) (map #(hash-map (first %) (str->int (second %))))
     (reduce merge) ;(map #(hash-map (keyword %1) %2))
     )

(def primitive-conv {:I 1 :V 5 :X 10 :L 50 :C 100 :D 500 :M 1000})

(def input-conv (atom {:glob :I :prok :V :pish :X :tegj :L}))

(def all-conv (merge primitive-conv @input-conv))

(def primitive? #(if (primitive-conv %) true false))

(fact "returns true if the symbol is a primitive"
      (primitive? :I) => true
      (primitive? :Z) => false)

(defn get-primitive
  [k]
  (cond
   (nil? k) nil
   (primitive? k) k
   :else (get-primitive (all-conv k))))

(fact "recursively gets the primitve value"
      (get-primitive :I) => :I
      (get-primitive :pish) => :X
      (get-primitive :hello) => nil)

(def sub-with-primitives #(map get-primitive %))

(fact "subs with primitives"
      (sub-with-primitives [:pish :tegj :glob :glob]) => [:X :L :I :I])

(defn sub-with-values
  [coll]
  (map #(cond
        (coll? %) (sub-with-values %)
        (primitive-conv %) (primitive-conv %)
        :else %)
       coll))

(sub-with-values '(+ :X (- :L :I) :I))

(def make-base-tree #(concat '(+) %))

(make-base-tree (list 1 2 3))

(defn substraction-rule
  [[f s & r]]
  (cond
   (nil? f) nil
   (nil? s) (cons f nil)
   (not (primitive-conv f)) (cons f (lazy-seq (substraction-rule (cons s r))))
   (< (primitive-conv f) (primitive-conv s)) (cons (list '- s f) (lazy-seq (substraction-rule r)))
   true (cons f (lazy-seq (substraction-rule (cons s r))))))

(substraction-rule '(+ :X :L :I :I))

(def transforms (atom []))

(defn add-transform
  [f]
  (swap! transforms #(conj % f)))

(defn transform
  [input]
  (reduce #(%2 %1) input @transforms))

(add-transform sub-with-primitives)

(add-transform make-base-tree)

(add-transform substraction-rule)

(add-transform sub-with-values)

(-> [:pish :tegj :glob :glob] transform eval
    )
