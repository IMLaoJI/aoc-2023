(ns aoc.2023.d10
  (:require [aoc.file-util :as f]
            [clojure.string :as str]))

(def input (f/read-lines "2023/d10.txt"))

(def grid (mapv vec input))

(def start-pos (first (for [row (range (count grid))
                            :let [col (.indexOf (grid row) \S)]
                            :when (not= col -1)]
                        [row col])))

(defn has-connection [r c direction]
  (let [pipe (get-in grid [r c])]
    (do
      (cond
        (= direction "north") (contains? #{\S \| \L \J} pipe)
        (= direction "east") (contains? #{\S \- \L \F} pipe)
        (= direction "west") (contains? #{\S \- \J \7} pipe)
        (= direction "south") (contains? #{\S \| \F \7} pipe)))))

(defn get-neighbors [r c]
  (let [neighbors []
        add-neighbor (fn [nr nc direction neis] (if (has-connection nr nc direction)
                                                  (conj neis [nr nc])))]
    (do
      (->> (or (when (and (> r 0) (has-connection r c "north")) (add-neighbor (- r 1) c "south" neighbors)) neighbors)
           (#(or (when (and (< c (- (count (first grid)) 1)) (has-connection r c "east")) (add-neighbor r (+ c 1) "west" %)) %))
           (#(or (when (and (> c 0) (has-connection r c "west")) (add-neighbor r (- c 1) "east" %)) %))
           (#(or (when (and (< r (- (count grid) 1)) (has-connection r c "south")) (add-neighbor (+ r 1) c "north" %)) %))))))

(defn bfs [start-pos]
  (loop [queue [[start-pos 0]]
         visited #{}
         result 0]
    (if (empty? queue)
      result
      (let [[[[r c] i] & rest] queue]
        (if (contains? visited [r c])
          (recur rest visited result)
          (let [visited' (conj visited [r c])
                result' (max result i)
                neighbors (get-neighbors r c)
                queue' (concat rest (map (fn [neighbor] [neighbor (+ i 1)]) neighbors))]
            (recur queue' visited' result')))))))

(defn part-1 []
  (bfs start-pos))



(defn part-2 [input]
  ())



(comment
  (time (part-1)))















