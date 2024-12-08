(ns aoc.2023.d18
  (:require [aoc.aoc :as aoc]
            [debux.core :refer :all]
            [clojure.string :as str]))

(def deltas {"R" [1 0] "D" [0 1] "L" [-1 0] "U" [0 -1]
             "0" [1 0] "1" [0 1] "2" [-1 0] "3" [0 -1]})


(defn parse-line [line]
  (let [[d n c] (str/split line #" ")
        c (str/replace c #"[(#)]" "")
        [dist dir] (map #(apply str %) (split-at 5 c))
        dist (Integer/parseInt dist 16)]
    {:p1 {:dir  (deltas d)
          :dist (parse-long n)}
     :p2 {:dir  (deltas dir)
          :dist dist}}))


(defn dig-trench [input]
  ; Shoelace formula + Pick's theorem
  (->> input
       (reduce (fn [{:keys [total x]}
                    {:keys [dir dist]}]
                 (let [[dx dy] dir
                       nx (+ x (* dist dx))
                       area #p(* nx (* dist dy))]
                   {:x     nx
                    :total (+ total area (/ dist 2))}))
               {:total 1
                :x     0})
       :total
       long))

(defn dig-trench-v2 [input]
  ; Shoelace formula + Pick's theorem
  (->> input
       (reduce (fn [{:keys [total x y]}
                    {:keys [dir dist]}]
                 (let [[dx dy] dir
                       nx (+  x (*  dist  dx))
                       ny (+  y (*  dist  dy))
                       area #p(* 0.5 (- (* x ny) (* nx y)))]
                   {:x     nx
                    :total (+ total area (/ dist 2))
                    :y ny}))
               {:total 1
                :x     0
                :y 0})
       :total
       long))


;todo 我可以通过一般的鞋带公式来验证下这个结论是否正确




(defn solve [input-file]
  (let [dig-plan (aoc/read-input input-file parse-line)]
    [(dig-trench-v2 (map :p1 dig-plan))
     (dig-trench-v2 (map :p2 dig-plan))]))

(defn solve-2 [input-file]
  (let [dig-plan (aoc/read-input input-file parse-line)]
    [(dig-trench (map :p1 dig-plan))
     (dig-trench (map :p2 dig-plan))]))



(comment
  (time (solve 18))
  (time (solve-2 18)))