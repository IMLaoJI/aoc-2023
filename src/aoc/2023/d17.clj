(ns aoc.2023.d17
  (:require [aoc.util :as u])
  (:import (java.util PriorityQueue)))

(def directions '[[0 1] [1 0] [0 -1] [-1 0]])

(defn init-queue []
  (let [states (PriorityQueue. 100000 (comparator (fn [x y] (<= (first x) (first y)))))]
    (.offer states '[0 [(0 0) 0 0]])
    (.offer states '[0 [(0 0) 1 0]])
    states))

(defn heat-loss [matrix i j di dj n]
  (reduce
    (fn [acc m]
      (+ acc (parse-long (str ((matrix (+ i (* m di))) (+ j (* m dj)))))))
    (range (inc n))))

(defn find-path [moves [start end] matrix]
  (loop [seen #{} result 0 moves moves]
    (if (not (empty? moves))
      (let [[loss [[i j] direction so-far :as state]] (.poll moves)
            [height width] [(count matrix) (count (first matrix))]
            current-directions (mapv (fn [n] [n (get directions n)])
                                     [(mod (inc direction) 4) (mod (+ direction 3) 4)])]
        (if (not (contains? seen state))
          (if (and (= i (dec height)) (= j (dec width)))
            (recur seen loss [])
            (let [seen' (conj seen state)
                  next-states (for [current current-directions
                                    n (range start (inc end))
                                    :let [[nd [di dj]] current
                                          ni (+ i (* n di))
                                          nj (+ j (* n dj))]
                                    :when (and (>= ni 0) (>= nj 0) (< ni height) (< nj width))]
                                (let [next-so-far (if (= direction nd) (inc so-far) 1)
                                      new-position [ni nj]
                                      next-loss (+ loss (heat-loss matrix i j di dj n))]
                                  [next-loss (list new-position nd next-so-far)]))]
              (doseq [ns next-states]
                (.add moves ns))
              (recur seen' result moves)))
          (recur seen result moves)))
      result)))
;由于没在单次迭代中一次性处理n步，导致缓慢
(defn solve
  [input-file]
  (->> (u/read-raw-input input-file)
       u/to-matrix
       (#(mapv (fn [flag] (find-path (init-queue) flag %)) [[1 3] [4 10]]))))

(comment
  (time (solve 17)))