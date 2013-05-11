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
        [tw.parsing :only [parse-input]]
        [tw.utils.time :only [time-to-string init-time add-mins]]
        [tw.print :only [print-tracks]]))
x
(def get-session-time #(- (% :end)
                          (% :start)))

(get-session-time {:start 9 :end 12 :talks []})

(def session-talks :talks)

(def talk-time #(-> % second weighted-num))

(talk-time [:a 30])

(def can-add-to-session? (can-add-to-bin? get-session-time session-talks talk-time))

(can-add-to-session? {:start 9 :end 12 :talks [[:a 30] [:b 30]]} [[:a 30]])
(can-add-to-session? {:start 9 :end 12 :talks [[:b 30] [:a 30] [:b 30] [:a 30] [:b 30]]} [[:a 30]])
(can-add-to-session? {:start 9 :end 12 :talks [[:a 30] [:b 30] [:a 30] [:b 30] [:a 30] [:b 30] [:b 30]]} [[:a 30]])

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

(def sort-talks (partial sort-by second))

(def date-format "hh:mm a")

(def dt-str (partial time-to-string date-format))

(defn add-time
  [session]
  (loop [agg-time (init-time (:start session)) talks (:talks session) agg-talks []]
    (if (seq talks)
      (let [[current-talk _ ] talks
            [ _ time ] current-talk
            new-agg-time (add-mins agg-time time)]
        (recur new-agg-time
               (rest talks)
               (conj agg-talks (conj current-talk (dt-str agg-time)))))
      (merge session {:talks agg-talks}))))

(def organizer-events {:lunch [:Lunch nil "12:00PM"]
                   :networking [(keyword "Networking Event") nil "05:00PM"]})

(def add-organizer-events
  (partial map (fn [{:keys [talks id] :as s}]
                  (if (even? id)
                    (assoc s :talks (conj talks (organizer-events :lunch)))
                    (assoc s :talks (conj talks (organizer-events :networking)))))))

(def assembled-tracks (-<> (parse-input text) (sort-talks) reverse (add-talks tracks <>) (map add-time <>) add-organizer-events print-tracks))
