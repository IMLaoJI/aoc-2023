(ns aoc.2023.d06
  (:require [aoc.file-util :as f]
            [clojure.string :as str]))

(def input (f/read-lines "2023/d06.txt"))

(defn parse-games [deal]
  (let [time-distance-part (map vector (first deal) (second deal))]
    (->> (map (fn [[time distance]]
                (reduce (fn [acc speed] (if (> (* speed (- time speed)) distance)
                                          (inc acc)
                                          acc)) 0 (take time (range)))) time-distance-part))))
(defn part-2 [input]
  (->> (map (comp list parse-long) (map str/join (map (partial re-seq #"\d+") input)))
       (parse-games)
       (first)))

(defn part-1 [input]
  (->> (map (partial map parse-long) (map (partial re-seq #"\d+") input))
       (parse-games)
       (apply *)))


(comment
  (println input)
  (time (parse-games input))
  (def deal (map parse-long (map str/join (map (partial re-seq #"\d+") input))))
  (map vector (first deal) (second deal))
  (time (part-1 input))
  (time (part-2 input)))

