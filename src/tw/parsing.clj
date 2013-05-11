(ns tw.parsing
  (:use [swiss-arrows.core :only [-< -<>]]
        [clojure.string :only [split]]))

(def text (slurp "src/tw/input.txt"))

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

(defn parse-input
  [text]
  (let [r (-< (split-by-nline text)
              (->> (map split-by-space) (map rest-last) (map #(interpose " " %)) (map #(apply str %)) (map keyword))
              (->> (map split-by-space) (map last) (map extract-int-string) (map str->int) (map words-to-mins)))]
    (map #(vector %1 %2) (first r) (second r))))

(parse-input text)
