(ns tw.core
  (:use [midje.sweet :only [fact facts truthy falsey]]
        [clojure.string :only [split]]
        [clojure.algo.generic.functor :only [fmap]]
        [clojure.set :only [union difference]]
        [swiss-arrows.core :only [-< -<>]]
        [clojure.pprint :only [pprint]]
        [clj-time.core :only [hours date-time plus minutes minute]]
        [clj-time.format :only [unparse formatter]]
        [table.core :only [table]]))

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

(get-total [[:a 30] [:b 30] [:a 30] [:b 30] [:a 30] [:b 30] [:b 30]])

(def morning-session {:start 9 :end 12 :talks []})

(def evening-session {:start 1 :end 5 :talks []})

(def track-def [morning-session evening-session])

(defn make-tracks
  [to-add]
  (let [tracks (flatten (repeat to-add track-def))]
    (->> (map #(merge (hash-map :id %2) %1) tracks (range))
         (into #{}))))

(def tracks (make-tracks 2))

(def weighted-num #(double (/ % 60)))

(def weighted-total #(-> % get-total weighted-num))

(weighted-total [[:a 30] [:b 60]])

(def get-session-time #(- (% :end)
                          (% :start)))

(get-session-time {:start 9 :end 12 :talks []})

(def can-add-to-session? (fn [[_ time] session]
                           ((some-fn pos? zero?) (- (get-session-time session)
                                                    (+ (weighted-num time) (weighted-total (session :talks)))))))

(def is-session-full? #(zero? (- (get-session-time %) (weighted-total (% :talks)))))

memmoize is-session-full?

(def is-better-fit? (fn [[_ time] session]
                      ))

check if the talks are getting near, if the time after adding this talk is lesser.

(when (not (is-session-full? session))
                        (is-better-fit? session))

(can-add-to-session? [:a 30] {:start 9 :end 12 :talks [[:a 30] [:b 30]]})
(can-add-to-session? [:a 30] {:start 9 :end 12 :talks [[:b 30] [:a 30] [:b 30] [:a 30] [:b 30]]})
(can-add-to-session? [:a 30] {:start 9 :end 12 :talks [[:a 30] [:b 30] [:a 30] [:b 30] [:a 30] [:b 30] [:b 30]]})

(defn add-talk-to-session
  [talk session]
  (->> (session :talks) (cons talk) (hash-map :talks) (merge session)))

(add-talk-to-session [:z 30] {:start 9 :end 12 :talks [[:a 30] [:b 30]]})

(defn add-talk
  [talk sessions]
  (when-let [session (->> (filter #(can-add-to-session? talk %) sessions) first)]
    (-> (difference sessions #{session}) (union #{(add-talk-to-session talk session)}))))

i have some talks [[:a 30] [:b 60] [:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60]]

loop through the items
loop through the bins
add the

(loop [[fi & ri] items bins b]
  (when fi
    (recur ri (loop [[fb & rb] bins]
                (when fb
                  (if (can-add? fi fb)
                    (add-to-bin fi fb)
                    (recur rb)))))))

(defn update-first
  [pred update]
  (fn [coll]
    (reduce
     (fn [acc x]
       (if (pred x)
         (reduced (concat (:res acc) (update x) (rest (:coll acc))))
         (assoc acc
           :res (conj (:res acc) x)
           :coll (rest (:coll acc)))))
     {:coll coll :res []}
     coll)))

loop through the items
add an item

(add-talk [:z 30] tracks)

(defn add-talks
  [talks sessions]
  (reduce #(add-talk %2 %1) sessions talks))

(add-talks [[:a 30] [:b 60] [:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60]] tracks)

(def sort-talks (partial into (sorted-set-by #(< (:id %1) (:id %2)))))

(defn init-time
  [hour]
  (date-time 2001 01 01 hour))

(defn add-mins
  [time mins]
  (plus time (minutes mins)))

(def custom-formatter (formatter "hh:mm a"))

(defn to-schedule
  [time]
  (unparse custom-formatter time))

(-> (init-time 11) (add-mins 45) (add-mins 30)
                                        ;(get-hour-min)
    )

(defn add-time
  [session]
  (loop [agg-time (init-time (:start session)) talks (:talks session) agg-talks []]
    (if (seq talks)
      (let [[current-talk _ ] talks
            [ _ time ] current-talk
            new-agg-time (add-mins agg-time time)]
        (recur new-agg-time
               (rest talks)
               (conj agg-talks (conj current-talk (to-schedule new-agg-time)))))
      (merge session {:talks agg-talks}))))

(init-time 9)

(map (fn [[a _ schedule]] (to-schedule schedule)) (add-time {:talks '([:Clojure 45] [:Ruby 60] [:Writing 60]),:start 9,:end 12,:id 0}))

(sort-by second <>) reverse

(def result (-<> (get-data text2) (add-talks tracks) (sort-talks) (map add-time <>)))

(pprint result)
(table result :style :unicode)

(def print-fn (fn [track track-num]
                (pprint (str "Track " track-num))))

(map #(vector (str "Track " %2) %1) (partition 2 result) (iterate inc 1))

(pprint (map #(vector (str "Track " %2) %1) (partition 2 result) (iterate inc 1)))

(def not-nil? (complement nil?))

(defn in-coll?
  [v coll]
  (not-nil? (some #{v} (flatten coll))))

(in-coll? :z [[[[:a 20] [:b :30]] [[:c 20] [:d :30]]]
              [[[:h 20] [:g :30]] [[:f 20] [:e :30]]]])


(def get-tracks #(repeat % empty-track))

(def num-of-tracks 2)

(get-tracks 2)

(def last-branch? (complement (comp sequential? first first)))

(last-branch? [[:a 20] [:b 30]])

(last-branch? [[[:a 20] [:b 30]]])

(def tracks-to-sessions #(filter last-branch?
                                 (tree-seq (complement last-branch?) identity %)))

(def bin {:capacity 10 :content []})

(def bins (repeat 2 bin))

(def items (take 20 (repeatedly (fn [] {:desc "abc" :weight (int (rand 100))}))))

(def bin-capacity :capacity)

(def bin-items :items)

(def item-wt :weight)

(sort-by fitem-weight items)

(defn agg
  [f]
  (fn [coll]
    (apply + (map f coll))))

((agg item-w) (take 5 (repeatedly (fn [] {:desc "abc" :weight (int (rand 100))}))))

(def sample-bin {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50} {:desc :a :weight 30}]})

(def sample-bins [{:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50} {:desc :a :weight 30}]}
                  {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50}]}
                  {:capacity 100 :items [{:desc :a :weight 10} {:desc :a :weight 50}]}])

(def sample-item1 {:desc :new :weight 10})

(def sample-item2 {:desc :new :weight 20})

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

(defn update-first
  [pred update]
  (fn [coll & p]
    (reduce
     (fn [acc x]
       (if (pred x p)
         (reduced (concat (:res acc) [(update x p)] (rest (:coll acc))))
         (assoc acc
           :res (conj (:res acc) x)
           :coll (rest (:coll acc)))))
     {:coll coll :res []}
     coll)))

(def add-item (update-first can-add-to-sample-bin? update-bin))

(add-item sample-bins sample-item1)

(def sample-items [{:desc :new-a :weight 10} {:desc :new-b :weight 20} {:desc :new-c :weight 20}])

(defn add-items
  [bins items]
  (reduce #(add-item %1 %2) bins items))

(add-items sample-bins (reverse (sort-by :weight sample-items)))
