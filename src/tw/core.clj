(ns tw.core
  (:use [midje.sweet :only [fact facts]]
        [swiss-arrows.core :only [-< -<>]]
        [tw.bin-packing :only [can-add-to-bin? add-items]]
        [tw.parsing :only [parse-input]]
        [tw.utils.time :only [time-to-string init-time add-mins]]
        [tw.print :only [print-tracks]]
        [clj-time.core :only [date-time]]))

;; functions for filling up the tracks with talks, using the bin packing way.
;; ##########################################################################

;; fn for sorting a seq of talks
(def sort-talks (partial sort-by second))

(fact "Sorts talks by the num of mins"
      (sort-talks [[:a 5] [:a 2] [:a 7]]) =>  [[:a 2] [:a 5] [:a 7]])

;; fn for calculating the allowed total time for a given session
(def get-session-time #(- (% :end) (% :start)))

(fact "Calculates the allowed session time given a session"
      (get-session-time {:start 9 :end 12 :talks []}) => 3)

;; fn for returning seq of talks of a given session
(def session-talks :talks)

(fact "returns the talks"
      (session-talks {:start 9 :end 12 :talks [[:a 20] [:b 30]]}) => [[:a 20] [:b 30]])

;; fn for calculating the weighted number
(def weighted-num #(double (/ % 60)))

(fact "returns the hours for mins equivalent"
      (weighted-num 120) => 2.0)

;; fn for returning the hours for a given talk
(def talk-time #(-> % second weighted-num))

(fact "returns the hours of a talk"
      (talk-time [:a 30]) => 0.5)

;; fn to check if a talk can be added to a given session
(def can-add-to-session? (can-add-to-bin? get-session-time session-talks talk-time))

(fact "returns true of the talk time fits into the remaining avalaible time, or returns false."
      (can-add-to-session? {:start 9 :end 12 :talks [[:a 30] [:b 30]]} [[:a 30]]) => true
      (can-add-to-session? {:start 9 :end 12 :talks [[:b 30] [:a 30] [:b 30] [:a 30] [:b 30]]} [[:a 30]]) => true
      (can-add-to-session? {:start 9 :end 12 :talks [[:a 30] [:b 30] [:a 30] [:b 30] [:a 30] [:b 30] [:b 30]]} [[:a 30]]) => false)

;; fn to add a talk to a session
(defn add-to-session
  [session [talk]]
  (assoc session :talks (conj (:talks session) talk)))

(fact "returns the session map with the talk added to it "
      (add-to-session {:talks []} [[:a 30]]) => {:talks [[:a 30]]})

;; fn for adding talks to a session. The add-items is a generic fn for 'bin -packing', which takes 3 fns as input and returns a fn which will take sessions and talks as input.
(def add-talks (add-items can-add-to-session? add-to-session sort-talks))

;; The input of the talks
(def text (slurp "src/tw/input.txt"))

;; map defining the structure of a session. The :start and :end keys specify the beginning and end hours of a session. While the : talk key contains a seq of the talks for that session.
(def session-types {:morning-session {:start 9 :end 12 :talks []}
                    :evening-session {:start 1 :end 5 :talks []}})

;; definition of a track, as a vector of morning and evening session.
(def track-def [(session-types :morning-session) (session-types :evening-session)])

;; fn for returning a seq of sessions based on the number of tracks. It adds an id to each session.
;; a seq of session is returned instead of tracks, as processing is simpler.
(defn make-tracks
  [n]
  (let [tracks (flatten (repeat n track-def))]
    (map #(merge (hash-map :id %2) %1) tracks (range))))

;; creates 2 tracks
(def tracks (make-tracks 2))

;; the date format that will be used to format the output string of the date.
(def date-format "hh:mm a")

(def dt-str (partial time-to-string date-format))

(fact "returns a formatted date string"
      (dt-str (date-time 2011 4 4 5)) => "05:00AM")

;; given a session, calculates the schedule for each talk and adds it to the talk vector.
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

;; vectors defining the organizer events which will be added to the morning and evening sessions.
(def organizer-events {:lunch [:Lunch nil "12:00PM"]
                       :networking [(keyword "Networking Event") nil "05:00PM"]})

;; adds the organizer events to a seq of sessions.
(def add-organizer-events
  (partial map (fn [{:keys [talks id] :as s}]
                  (if (even? id)
                    (assoc s :talks (conj talks (organizer-events :lunch)))
                    (assoc s :talks (conj talks (organizer-events :networking)))))))

;; assembles the tracks by - parsing the input, adding the talks to the tracks, adding scheduled time, adding the organizer events and finally printing the output.
(defn assemble-tracks
  []
  (-<> (parse-input text) (add-talks tracks <>) (map add-time <>) add-organizer-events print-tracks))
