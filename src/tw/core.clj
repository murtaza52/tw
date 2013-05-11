(ns tw.core
  (:use [midje.sweet :only [fact facts truthy falsey]]
        [clojure.string :only [split]]
        [clojure.set :only [union difference]]
        [swiss-arrows.core :only [-< -<>]]
        [clojure.pprint :only [pprint]]
        [clj-time.core :only [hours date-time plus minutes minute]]
        [clj-time.format :only [unparse formatter]]
        [table.core :only [table]]
        [tw.bin-packing :only [can-add-to-bin? add-items]]
        [tw.parsing :only [parse-input]]))

(def get-session-time #(- (% :end)
                          (% :start)))

(get-session-time {:start 9 :end 12 :talks []})

(def session-talks :talks)

(def talk-time #(-> % second weighted-num))

(talk-time [:a 30])

(def can-add-to-session? (can-add-to-bin? get-session-time session-talks talk-time))

(defn add-to-session
  [session [talk]]
  (assoc session :talks (conj (:talks session) talk)))

(add-to-session {:talks []} [[:a 30]])

(def add-talks (add-items can-add-to-session? add-to-session))

(def text (slurp "src/tw/input.txt"))

(def morning-session {:start 9 :end 12 :talks []})

(def evening-session {:start 1 :end 5 :talks []})

(def track-def [morning-session evening-session])

(defn make-tracks
  [n]
  (let [tracks (flatten (repeat n track-def))]
    (map #(merge (hash-map :id %2) %1) tracks (range))))

(def tracks (make-tracks 2))

(def weighted-num #(double (/ % 60)))

(add-talks tracks (parse-input text))


;; (def weighted-total #(-> % get-total weighted-num))

;; (weighted-total [[:a 30] [:b 60]])

;; (def can-add-to-session? (fn [[_ time] session]
;;                            ((some-fn pos? zero?) (- (get-session-time session)
;;                                                     (+ (weighted-num time) (weighted-total (session :talks)))))))

;; (def is-session-full? #(zero? (- (get-session-time %) (weighted-total (% :talks)))))

(can-add-to-session? {:start 9 :end 12 :talks [[:a 30] [:b 30]]} [[:a 30]])
(can-add-to-session? {:start 9 :end 12 :talks [[:b 30] [:a 30] [:b 30] [:a 30] [:b 30]]} [[:a 30]])
(can-add-to-session? {:start 9 :end 12 :talks [[:a 30] [:b 30] [:a 30] [:b 30] [:a 30] [:b 30] [:b 30]]} [[:a 30]])

;; (defn add-talk-to-session
;;   [talk session]
;;   (->> (session :talks) (cons talk) (hash-map :talks) (merge session)))

;; (add-talk-to-session [:z 30] {:start 9 :end 12 :talks [[:a 30] [:b 30]]})

;; (defn add-talk
;;   [talk sessions]
;;   (when-let [session (->> (filter #(can-add-to-session? talk %) sessions) first)]
;;     (-> (difference sessions #{session}) (union #{(add-talk-to-session talk session)}))))

;; (add-talk [:z 30] tracks)

;; (defn add-talks
;;   [talks sessions]
;;   (reduce #(add-talk %2 %1) sessions talks))

;; (add-talks [[:a 30] [:b 60] [:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60][:a 30] [:b 60]] tracks)

;;(def sort-talks (partial into (sorted-set-by #(< (:id %1) (:id %2)))))

(def sort-talks (partial sort-by second))

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
               (conj agg-talks (conj current-talk (to-schedule agg-time)))))
      (merge session {:talks agg-talks}))))

(map (fn [[a _ schedule]] (to-schedule schedule)) (add-time {:talks '([:Clojure 45] [:Ruby 60] [:Writing 60]),:start 9,:end 12,:id 0}))

(def lunch [:Lunch nil "12:00 PM"])

(def networking [(keyword "Networking Event") nil "05:00 PM"])

(def add-organising-events
  (partial map (fn [{:keys [talks id] :as s}]
                  (if (even? id)
                    (assoc s :talks (conj talks lunch))
                    (assoc s :talks (conj talks networking))))))

(def assemble-tracks (-<> (parse-input text) (sort-talks) reverse (add-talks tracks <>) (map add-time <>) add-organising-events))

(pprint result)
(table result :style :unicode)

(def print-fn (fn [track track-num]
                (pprint (str "Track " track-num))))

(map #(vector (str "Track " %2) %1) (partition 2 result) (iterate inc 1))

(pprint (map #(vector (str "Track " %2) %1) (partition 2 result) (iterate inc 1)))
