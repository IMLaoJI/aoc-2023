(ns aoc.2023.d04
  (:require [aoc.file-util :as f]
            [clojure.string :as str]))

(def input (f/read-lines "2023/d04.txt"))

(defn parse-games [s]
  (let [card (rest (re-find #"Card\s+(\d+)\s*:([^|]+)[|]([^$]+)$" s))
        deal-number (->> (rest card)
                         (map str/trim)
                         (map #(str/split % #"\s+"))
                         (map (partial map parse-long)))]
    (hash-map :card-id (parse-long (nth card 0))
              :winning (first deal-number)
              :guesses (second deal-number))))

(defn calc-score [game]
  (reduce (fn [acc e] (if (some #(= % e) (:winning game))
                        (if (= acc 0)
                          1
                          (* acc 2))
                        acc)
            ) 0 (:guesses game)))

(defn count-intersect [card]
  (count (clojure.set/intersection (set (:guesses card)) (set (:winning card)))))

(defn update-map [cards map card]
  (let [count (count-intersect card)
        won-this (if-let [val (get map (:card-id card))]
                   (inc val)
                   1)
        new-map (assoc map (:card-id card) won-this)
        rest-cards (take count (drop (:card-id card) cards))]

    (reduce (fn [m c]
              (update m (:card-id c) #(if % (+ % won-this) won-this)))
            new-map rest-cards)))

(defn part-1 [input]
  (->> (map parse-games input)
       (map calc-score)
       (reduce +)))

(defn part-2 [input]
  (let [cards (map parse-games input)]
    (->>
      cards
      (reduce (partial update-map cards) {})
      (vals)
      (apply +))))



(comment
  (->> input
       (map parse-games))
  ;(part-1 input)
  (part-2 input)
  (take 4 (drop 2 (map parse-games input)))
  (calc-score (parse-games (first input))))
