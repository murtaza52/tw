;; This ns contains fns for parsing the text input to a seq of talks.
(ns tw.parsing
  (:use [swiss-arrows.core :only [-< -<>]]
        [tw.utils.misc :only [freturn]]
        [tw.utils.text :only [split-by-space split-by-nline extract-int-string str->int]]
        [midje.sweet :only [fact facts truthy]]))

;; text input
(def text (slurp "src/tw/input.txt"))

(def words-to-mins-hash {"lightning" 5})

;; fn which returns the the string from the words-to-mins-hash or returns the mins.
(def words-to-mins #(freturn (words-to-mins-hash %) %))

(fact "returns mins for given string"
      (words-to-mins "lightning") => 5
      (words-to-mins 20) => 20)

;; parses the input into a seq of vector of talks
(defn parse-input
  [text]
  (let [r (-< (split-by-nline text)
              (->> (map split-by-space) (map drop-last) (map #(interpose " " %)) (map #(apply str %)) (map keyword))
              (->> (map split-by-space) (map last) (map extract-int-string) (map str->int) (map words-to-mins)))]
    (map #(vector %1 %2) (first r) (second r))))

(facts "sucessfully parses the input"
       (parse-input text) => truthy)
