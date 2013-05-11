(ns tw.print)

(def print-talk (fn [[title time schedule]]
                  (println schedule (name title) (cond
                                                  (nil? time) ""
                                                  (= 5 time) "lightning"
                                                  :else (str time "min")))))

(print-talk [:Lunch nil "12:00PM"])

(print-talk [(keyword "Ata de Scala") 30 "11:00PM"])

(def print-session #(doseq [talk (:talks %)]
                     (print-talk talk)))

(def print-track #(doseq [session %]
                   (print-session session)))

(def print-tracks #(doseq [[track counter] (map (fn [t c] [t c]) (partition 2 %) (iterate inc 1))]
                     (do
                       (println)
                       (println (str "Track " counter ":"))
                       (print-track track))))
