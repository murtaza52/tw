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

(def lunch [:Lunch nil "12:00 PM"])

(def networking [(keyword "Networking Event") nil "05:00 PM"])

(def add-organising-events
  (partial map (fn [{:keys [talks id] :as s}]
                  (if (even? id)
                    (assoc s :talks (conj talks lunch))
                    (assoc s :talks (conj talks networking))))))

(def assembled-tracks (-<> (parse-input text) (sort-talks) reverse (add-talks tracks <>) (map add-time <>) add-organising-events))

(def print-talk (fn [[title time schedule]]
                  (println schedule (name title) (cond
                                                  (nil? time) ""
                                                  (= 5 time) "lightning"
                                                  :else (str time "min")))))

(pprint assembled-tracks)

(print-talk [:Lunch nil "12:00PM"])

(print-talk [(keyword "Ata de Scala") 30 "11:00PM"])

(def print-session #(doseq [talk (:talks %)]
                     (print-talk talk)))

(def sample-session
 {:talks
  [[:A 30 "01:00 AM"]
   [:Ruby 30 "01:30 AM"]
   [:Programming 30 "02:00 AM"]
   [:Sit
 30 "02:30 AM"]
   [:Woah 30 "03:00 AM"]
   [:Lua 30 "03:30 AM"]
   [:Rails 5 "04:00 AM"]
   [:Networking nil "05:00 PM"]],
  :start 1,
  :end 5,
  :id 3})

(print-session sample-session)

(def print-track #(doseq [session %]
                   (print-session session)))

(doseq [[track counter] (map (fn [t c] [t c]) (partition 2 assembled-tracks) (iterate inc 1))]
  (do
    (println)
    (println (str "Track " counter ":"))
    (print-track track)))

(pprint result)
(table result :style :unicode)

(def print-fn (fn [track track-num]
                (pprint (str "Track " track-num))))

(do (map (fn [track num] (println (str "Track " num)))
     (partition 2 assembled-tracks)
     (iterate inc 1)))


(pprint (map #(vector (str "Track " %2) %1) (partition 2 result) (iterate inc 1)))
