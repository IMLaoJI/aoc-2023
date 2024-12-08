(ns aoc.2023.d02
  (:require [aoc.file-util :as f]
            [aoc.matrix :as matrix]
            [clojure.string :as str]))

(def input (f/read-lines "2023/d02.txt"))

(defn parse-game
  "解析单行文本到一个以id为key,[r g b ]的vector为value的hashmap"
  [s]
  (let [[_ id] (re-find #"^Game (\d+):" s)
        pulls (str/split s #";")]
    (hash-map (parse-long id)
              (vec (for [pull pulls
                         :let [[_ r] (re-find #"(\d+) red" pull)
                               [_ g] (re-find #"(\d+) green" pull)
                               [_ b] (re-find #"(\d+) blue" pull)]]
                     [
                      ((fnil parse-long "0") r)
                      ((fnil parse-long "0") g)
                      ((fnil parse-long "0") b)])))))




(defn parse-games [games] (apply merge (map parse-game games)))

(defn possible?
  "确认每个[r g b]都满足指定的max-pull"
  [max-pull [_ pulls]]
  (every? true? (mapcat #(map >= max-pull %) pulls)))



(defn power
  "返回一场游戏中最大的红绿蓝值的乘积"
  [[_ pulls]]
  (reduce * (map (partial apply max) (matrix/transpose pulls))))



(defn part-1 [input]
  (->> (parse-games input)
       (filter (partial possible? [12 13 14]))
       (map first)
       (reduce +)))


(defn part-2 [input]
  (->> (parse-games input)
       (map power)
       (reduce +)))



(comment
  ;(possible? [12 22 33]  [1 [[2 1 6] [4 1 3] [0 5 7] [2 1 6]]])
  ;(first (parse-games input))
  (part-1 input)
  (power (first (parse-games input)))
  (part-2 input))

