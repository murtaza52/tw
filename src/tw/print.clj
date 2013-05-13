;; This ns contains fns for printing the tracks.
(ns tw.print
  (:use [midje.sweet :only [fact facts truthy]]))


;; The facts / tests in this ns have been commented out to prevent their printing when the repl starts.

;; fn for printing a given talk vector
(def print-talk (fn [[title time schedule]]
                  (println schedule (name title) (cond
                                                  (nil? time) ""
                                                  (= 5 time) "lightning"
                                                  :else (str time "min")))))

;; (fact "prints a talk successfully"
;;       (print-talk [:Lunch nil "12:00PM"]) => nil
;;       (print-talk [(keyword "Ata de Scala") 30 "11:00PM"]) => nil)

;; fn for printing a given session
(def print-session #(doseq [talk (:talks %)]
                     (print-talk talk)))

;; (fact "prints a session successfully"
;;       (print-session {:start 9 :end 12 :talks [[:a 20 "a"] [:b 30 "b"]]}) => nil)

;; fn for printing a given track
(def print-track #(doseq [session %]
                   (print-session session)))

;; (fact "prints a track"
;;       (print-track [{:start 9 :end 12 :talks [[:a 20 "a"] [:b 30 "b"]]} {:start 4 :end 5 :talks [[:a 20 "a"] [:b 30 "b"]]}]) => nil)

;; fn for printing given tracks
(def print-tracks #(doseq [[track counter] (map (fn [t c] [t c]) (partition 2 %) (iterate inc 1))]
                     (do
                       (println)
                       (println (str "Track " counter ":"))
                       (print-track track))))

;; (fact "prints a track"
;;       (print-tracks [{:start 9 :end 12 :talks [[:a 20 "a"] [:b 30 "b"]]} {:start 4 :end 5 :talks [[:a 20 "a"] [:b 30 "b"]]}
;;                      {:start 9 :end 12 :talks [[:a 20 "a"] [:b 30 "b"]]} {:start 4 :end 5 :talks [[:a 20 "a"] [:b 30 "b"]]}]) => nil)
