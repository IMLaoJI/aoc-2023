(ns aoc.2023.d09
  (:require [aoc.file-util :as f]
            [clojure.string :as str]))

(def input (f/read-lines "2023/d09.txt"))

(defn parse-input [input]
  (mapv (partial mapv parse-long) (mapv #(str/split % #" ") input)))

(defn solve-common [f s]
  (if (every? zero? s) 0 (f (solve-common f (rest (map - s (cons 0 s)))) s)))

(defn part-1 [input]
  (->> (parse-input input)
       (map (partial solve-common (fn [diff s] (+ (last s) diff))))
       (reduce +)))

(defn part-2 [input]
  (->> (parse-input input)
       (map (partial solve-common (fn [diff s] (- (first s) diff))))
       (reduce +)))








(+ 2 23)
(comment
  (time (part-1 input))
  (time (part-2 input))
  (solve-2 [10 13 16 21 30 45])
  + 3 3
  (+ 2 2))




(defn solve [s]
  (if (every? zero? s) 0 (+ (solve (rest (map - s (cons 0 s)))) (last s))))

(defn solve-2 [s]
  (if (every? zero? s)
    0 (- (first s) (solve-2 (rest (map - s (cons 0 s)))))))



