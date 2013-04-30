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
              (->> (map split-by-space) (map rest-last) (map #(interpose " " %)) (map #(apply str %)) (map keyword))
              (->> (map split-by-space) (map last) (map extract-int-string) (map str->int) (map words-to-mins)))]
    (map #(vector %1 %2) (first r) (second r))))

(get-data text2)

(pprint (get-data text2))

(defn get-total
  [coll]
  (reduce #(+ %1 (second %2)) 0 coll))

(def track-def [{:start 9 :end 12 :talks []} {:start 1 :end 5 :talks []}])



(def weighted-num #(double (/ % 60)))

(def weighted-total #(-> % get-total weighted-num))

(weighted-total [[:a 30] [:b 60]])

(def session-time #(- (second %)
                      (first %)))

(def get-tracks (partial partition 2))

(get-tracks '([:a 20] [:b 30] [:c 20] [:d 30] [:h 20] [:g 30] [:f 20] [:e 30]))

(defn which-session?
  [talk sessions]
  (some #(cond
          (= (first %) talk) 0
          (= (second %) talk) 1
          :else nil)
        (get-tracks sessions)))

(fact "A"
      (which-session? [:d 30] '([:a 20] [:b 30] [:c 20] [:d 30] [:h 20] [:g 30] [:f 20] [:e 30])) => 2
      (which-session? [:h 20] '([:a 20] [:b 30] [:c 20] [:d 30] [:h 20] [:g 30] [:f 20] [:e 30])) => 1
      (which-session? [:z 30] '([:a 20] [:b 30] [:c 20] [:d 30] [:h 20] [:g 30] [:f 20] [:e 30])) => nil)

(session-time [9 12])

(def session-format (fn [talk sessions]
                      (->> (which-session? talk sessions) (get track-def))))

(session-format [:b 30] '([:a 20] [:b 30] [:c 20] [:d 30] [:h 20] [:g 30] [:f 20] [:e 30]))

(def get-talk-time #(second %))

(get-talk-time [:a 30])

(def get-talk-title #(first %))

(def can-add-to-session? (fn [talk session]
                           (let [time-till-now (weighted-total session)
                                 talk-time (weighted-num (get-talk-time talk))
                                 allowed-session-time ])))

(get track-def 1)

(defn add-to-session
  [talk session]
  ())

(defn exists-in-session?
  [session talk]
  (= talk (first session)))

(fact "A"
      (exists-in-session? [:a 20] :a) => true
      (exists-in-session? [:a 20] :c) => false)

;; (defn exists-in-tracks?
;;   [tracks talk]
;;   (empty? (for [track tracks
;;                sessions track
;;                session sessions
;;                :when (exists-in-session? session talk)
;;                :while (exists-in-session? session talk)]
;;            true)))

(def not-nil? (complement nil?))

(defn in-coll?
  [v coll]
  (not-nil? (some #{v} (flatten coll))))

(in-coll? :z [[[[:a 20] [:b :30]] [[:c 20] [:d :30]]]
              [[[:h 20] [:g :30]] [[:f 20] [:e :30]]]])

(def empty-track [[] []])

(def add-track (partial cons empty-track))

(defn session?
  [v]
  (if (vector? v)
    (and (keyword? (first v)) (integer? (second v)))
    false))

(facts "A"
       (session? [:a 60]) => true
       (session? :b) => false
       (session? 20) => false)

(defn get-sessions
  [tracks]
  (filter session? (tree-seq sequential? identity tracks)))

(pprint (get-sessions [[[[:a 20] [:b 30]] [[:c 20] [:d 30]]]
                       [[[:h 20] [:g 30]] [[:f 20] [:e 30]]]]))

(get-sessions [[[[:a 20] [:b 30]] [[:c 20] [:d 30]]]
               [[[:h 20] [:g 30]] [[:f 20] [:e 30]]]])



;; (defn assemble-tracks
;;   [talks]
;;   (loop [[f & r] talks tracks empty-track]
;;     (cond
;;      (nil? f) tracks
;;      :else (recur r (add-to-track f tracks)))))

(def talks (get-data text2))

(def get-tracks #(repeat % empty-track))

(def num-of-tracks 2)

(get-tracks 2)

(def last-branch? (complement (comp sequential? first first)))

(last-branch? [[:a 20] [:b 30]])

(last-branch? [[[:a 20] [:b 30]]])

(def tracks-to-sessions #(filter last-branch?
                                 (tree-seq (complement last-branch?) identity %)))

(fact "a"
      (tracks-to-sessions (get-tracks 2))
      (tracks-to-sessions [[[[:a 30] [:c 30]]] [[[:e 30] [:f 30]]]]))

(can-add-to-sessions )

(not-empty [:e])

(defn add-talk
  [talk tracks]
  (cons talk tracks))

(reduce (fn [tracks talk]
          (add-talk talk tracks))
        (-> num-of-tracks get-tracks tracks-to-sessions)
        talks)


(assemble-tracks d)

(def conversions (slurp "src/tw/conversions.txt"))

(->> conversions split-by-nline (map split-by-tab) (into {}) (map #(hash-map (first %) (str->int (second %))))
     (reduce merge) ;(map #(hash-map (keyword %1) %2)))

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

(-> [:pish :tegj :glob :glob] transform eval)
