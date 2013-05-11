(ns tw.utils.time
  (:use [clj-time.core :only [hours date-time plus minutes minute]]
        [clj-time.format :only [unparse formatter]]
        [midje.sweet :only [fact facts]]
        [tw.utils.text :only [remove-space]]))

(defn init-time
  [hour]
  (date-time 2001 01 01 hour))

(defn add-mins
  [time mins]
  (plus time (minutes mins)))

(defn time-to-string
  [format time]
  (remove-space (unparse (formatter format) time)))

(-> (init-time 11) (add-mins 45) (add-mins 30)
                                        ;(get-hour-min)
    )
