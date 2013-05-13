(ns tw.utils.text
  (:use [tw.utils.misc :only [freturn]]
        [clojure.string :only [split]]
        [midje.sweet :only [fact facts]]))

(def split-by-nline #(split % #"\n"))

(fact "Splits lines"
      (split-by-nline "Ruby Errors from Mismatched Gem Versions 45min \n Common Ruby Errors 45min")
      => ["Ruby Errors from Mismatched Gem Versions 45min" "Common Ruby Errors 45min"])

(def split-by-tab #(split % #"\t"))

(fact "splits by tabs"
      (split-by-tab "a\tb") => ["a" "b"])

(def split-by-space #(split % #" "))

(fact "splits by spaces"
      (split-by-space "a b c") => ["a" "b" "c"])

(def remove-space #(->> (split-by-space %) (apply str)))

(remove-space "05:00 AM")

(defn extract-int-string
  [str]
  (freturn (->> str (re-matches (re-pattern "(\\d+).*")) second) str))

(fact "returns the int part of the string"
      (extract-int-string "24min") => "24")

(freturn (extract-int-string "m24min") "lightning")

(defn str->int
  [str]
  (freturn (if (re-matches (re-pattern "\\d+") str) (read-string str))
           str))

(fact "parses the string to an int"
      (str->int "25") => 25
      (str->int "gh4") => nil)
