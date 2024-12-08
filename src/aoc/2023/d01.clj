(ns aoc.2023.d01
  (:require
    [aoc.file-util :as f]
    [clojure.string :as str]))

(def input (f/read-lines "2023/d01.txt"))

(def word-map {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5"
               "six" "6" "seven" "7" "eight" "8" "nine" "9"})

(defn common-solve
  [re s]
  (->> (re-seq re s)
       ((juxt first last))
       (map second)
       (map #(get word-map % %))
       (apply str)
       parse-long))

(defn solve [input re]
  (reduce + 0 (map #(common-solve re %) input)))

(defn part-1 [input]
  (solve input #"(\d)"))

(defn part-2 [input]
  (solve input (re-pattern (str "(?=(\\d|" (str/join "|" (keys word-map)) "))"))))

(comment
  (part-1 input)
  (+ 2 2)
  (part-2 input))











