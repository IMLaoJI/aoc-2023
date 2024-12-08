(ns aoc.2023.d14
  (:require [aoc.aoc :as aoc]))

(defn deal [grid width height [y x]]
  (loop [grid grid]
    (let [grid' (reduce (fn [v [i j]]
                          (if (= (v [j i]) \O)
                            (let [new-i (+ i y) new-j (+ j x)]
                              (if (and (>= new-i 0) (< new-i height) (>= new-j 0)
                                       (< new-i width) (= (v [new-j new-i]) \.))
                                (assoc v [new-j new-i] \O [j i] \.) v))
                            v)) grid (for [i (range height)
                                           j (range width)]
                                       [j i]))]
      (if (= grid grid') grid (recur grid')))))

(defn common-calc [height s]
  (->> s
       (keep (fn [[k v]] (when (= v \O) k)))
       (map (comp #(- height %) second))
       (reduce +)))

(defn solve [input-file]
  (let [lines (aoc/read-input input-file)
        [width height] [(count (first lines)) (count lines)]
        points (aoc/grid->points lines)]

    [(->> (reduce #(deal %1 width height %2) points [[-1 0]])
          ((partial common-calc height)))
     (->> (loop [last_boards [] grid points i 0]
            (if (< i 1e9)
              (do
                (if (> (count last_boards) 100)
                  (subvec last_boards 1))
                (let [last_boards (conj last_boards grid)
                      new_grid (reduce #(deal %1 width height %2) grid [[-1 0] [0 -1] [1 0] [0 1]])]
                  (if (contains? (set last_boards) new_grid)
                    (let [distance (- (count last_boards) (.indexOf last_boards new_grid))]
                      (recur [] new_grid (+ i (+ 1 (* (quot (- 1e9 i) distance) distance)))))
                    (recur last_boards new_grid (inc i)))))
              grid))
          ((partial common-calc height)))]))

(comment
  (time (solve 14)))

