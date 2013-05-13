;; This ns contains fns for dealing with time
(ns tw.utils.time
  (:use [clj-time.core :only [hours date-time plus minutes minute]]
        [clj-time.format :only [unparse formatter]]
        [midje.sweet :only [fact facts]]
        [tw.utils.text :only [remove-space]]))

;; fn for creating a date-time object given a hour.
(defn init-time
  [hour]
  (date-time 2001 01 01 hour))

(fact "returns a date object"
      (init-time 2) => (date-time 2001 01 01 2))

(defn add-mins
  [time mins]
  (plus time (minutes mins)))

(fact "adds mins to a given time"
      (add-mins (date-time 2001 01 01 2) 30) => (date-time 2001 01 01 2 30))

(defn time-to-string
  [format time]
  (remove-space (unparse (formatter format) time)))

(fact "formats the date based on the given time"
      (time-to-string "hh:mm" (date-time 2001 01 01 2 30)) => "02:30")
