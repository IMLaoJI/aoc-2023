(ns aoc.2023.d14-1
  (:require [aoc.util :as u]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))
(use 'debux.core)

;; Count the `O` characters in a given row.
(defn- count-rocks [row]
  (count (filter #(= % \O) row)))

;; Calculate the load for a given "field". Do this by processing the rows of
;; `matrix` in reverse order while also incrementing a counter.
(defn- count-load [matrix]
  (reduce + (map #(* %2 (count-rocks %1))
                 (reverse matrix) (range 1 (inc (count matrix))))))

;; Roll all the rocks in `row` towards the left edge.
(defn- roll-left [row]
  (loop [row row]
    (let [row' (reduce (fn [v i]
                         (let [c1 (v i), c0 (v (dec i))]
                           (if (and (= c1 \O) (= c0 \.))
                             (assoc v (dec i) \O i \.)
                             v)))
                       row (range 1 (count row)))]
      (if (= row row') row (recur row')))))

;; Roll all the rocks in `row` towards the right edge.
(defn- roll-right [row]
  (loop [row row]
    (let [row' (reduce (fn [v i]
                         (let [c1 (v i), c0 (v (inc i))]
                           (if (and (= c1 \O) (= c0 \.))
                             (assoc v (inc i) \O i \.)
                             v)))
                       row (reverse (range 0 (dec (count row)))))]
      (if (= row row') row (recur row')))))

;; Tilt the field north. Do this by transposing `matrix`, doing a `roll-left`,
;; and transposing it back.
(defn- tilt-north [matrix]
  (u/transpose (mapv roll-left (u/transpose matrix))))

;; Tilt the field north. Do this by transposing `matrix`, doing a `roll-right`,
;; and transposing it back.
(defn- tilt-south [matrix]
  (u/transpose (mapv roll-right (u/transpose matrix))))

;; Tilt the field west. This is just a `roll-left` operation, no transposing
;; needed.
(defn- tilt-west [matrix]
  (mapv roll-left matrix))

;; Tilt the field east. This is just a `roll-right` operation, no transposing
;; needed.
(defn- tilt-east [matrix]
  (mapv roll-right matrix))

(defn part-1
  "Day 14 Part 1"
  [input]
  (->> (u/read-raw-input input)
       u/to-matrix
       (tilt-north)
       count-load))


(comment
  (time (part-1 14)))


;; Do one full spin of `matrix`.
(defn- spin-once [matrix]
  (->> matrix
       tilt-north
       tilt-west
       tilt-south
       tilt-east))

;; Once the cycle is detected, determine which of the cached fields in `seen`
;; corresponds to the `target` index.
(defn- find-nth [seen location n target]
  (let [cycle (- n location)
        mapping (set/map-invert seen)
        remain (mod (- target n) cycle)]
    (mapping (+ remain location))))

;; Cycle the spinning of the field until we see the same configuration a second
;; time. When that happens, call `find-nth` to get the cycle length and map to
;; the `seen` configuration.
(defn- spin-cycle [target matrix]
  (loop [[m & ms] (iterate spin-once matrix), idx 0, seen {}]
    (if (seen m)
      (find-nth seen (seen m) idx target)
      (recur ms (inc idx) (assoc seen m idx)))))

(defn part-2
  "Day 14 Part 2"
  [input]
  (->> (u/read-raw-input input)
       u/to-matrix
       (spin-cycle 1000000000)
       count-load))
(comment
  (part-2 14))


(defn shift [s]
  (loop [n 0 s s result ""]
    (if (empty? s)
      (str result (apply str (repeat n \.)))
      (let [[c & s'] s]
        (cond
          (= c \.) (recur (inc n) s' result)
          (= c \O) (recur n s' (str result c))
          (= c \#) (recur 0 s' (str result (apply str (repeat n \.)) c)))))))

(defn load [xs]
  (reduce + (map-indexed #(* (- (count xs) %1) (count (filter (fn [param1] (= \O param1)) %2))) xs)))

(defn find-cycle [outs]
  (loop [seen {} i 0 [x & xs] outs]
    (let [h (hash (str/join x))]
      (if-let [j (seen h)]
        [j i]
        (recur (assoc seen h i) (inc i) xs)))))

(defn process [s]
  (->> s
       (map (comp reverse shift))
       (apply map vector)))

(defn times [n f x]
  (if (<= n 0)
    x
    (recur (dec n) f (f x))))

(defn cycle-tilt [s]
  (times 4 process s))

(defn -main []
  (let [input (->> "2023/d14.txt"
                   io/resource
                   (slurp)
                   (str/split-lines)
                   (apply map vector))
        outs (iterate cycle-tilt input)
        [start next] (find-cycle outs)
        i (+ start (mod (- 1000000000 start) (- next start)))]
    (do (println (load (u/transpose (map shift input))))
        (println  next (load (u/transpose (nth outs i)))))))

(time (-main))






