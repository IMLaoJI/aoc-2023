(ns aoc.2023.d14-2
  (:require [aoc.util :as u]
            [clojure.set :as set]))

(defn count-weight
  [matrix]
  (reduce + (map-indexed #(* (- (count matrix) %1) (count (filter (fn [p] (= p \O)) %2))) matrix)))


(defn transfer-left
  [matrix]
  (loop [matrix matrix]
    (let [matrix' (reduce (fn [acc x]
                            (if (and (= (acc x) \O) (= (acc (dec x)) \.))
                              (assoc acc x \. (dec x) \O)
                              acc)) matrix (range 1 (count matrix)))]
      (if (= matrix matrix') matrix' (recur matrix')))))

(defn transfer-right
  [matrix]
  (loop [matrix matrix]
    (let [matrix' (reduce (fn [acc x]
                            (if (and (= (acc x) \O) (= (acc (inc x)) \.))
                              (assoc acc x \. (inc x) \O)
                              acc)) matrix (range 0 (dec (count matrix))))]
      (if (= matrix matrix') matrix' (recur matrix')))))

(defn transfer-north
  "往北就是先转置矩阵后再向右边倾倒,最后在转置回来"
  [matrix]
  (u/transpose (mapv transfer-left (u/transpose matrix))))


(defn transfer-south
  [matrix]
  (u/transpose (mapv transfer-right (u/transpose matrix))))

(defn transfer-west
  [matrix]
  (mapv transfer-left matrix))

(defn transfer-east
  [matrix]
  (mapv transfer-right matrix))

(defn cycle-matrix
  [matrix]
  (->> (transfer-north matrix)
       transfer-west
       transfer-south
       transfer-east))

(defn find-nth
  [seen location idx rounds]
  (let [distance (- idx  location)
        remain  (mod (- rounds idx) distance)
        revert-map  (set/map-invert seen)]
     (revert-map (long (+ location remain)))))


(defn deal-cycle
  [rounds matrix]
  (loop [[m & ms] (iterate cycle-matrix matrix) idx 0 seen {}]
    (if (seen m)
      (find-nth seen (seen m) idx rounds)
      (recur ms (inc idx) (assoc seen m idx)))))


(defn solve
  [input-file]
  (let [matrix (->> (u/read-raw-input input-file)
                    u/to-matrix)]
    [(->> (transfer-north matrix)
          count-weight)
     (->> (deal-cycle 1e9 matrix)
          count-weight)]))

(time (solve 14))



