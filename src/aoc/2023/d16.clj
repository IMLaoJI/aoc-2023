(ns aoc.2023.d16
  (:require [aoc.util :as u]))

(defn next-move
  [[x y] direction matrix]
  (case direction
    "east" (case ((matrix y) x)
             \. [["east" [(inc x) y]]]
             \- [["east" [(inc x) y]]]
             \| [["north" [x (dec y)]] ["south" [x (inc y)]]]
             \\ [["south" [x (inc y)]]]
             \/ [["north" [x (dec y)]]])
    "west" (case ((matrix y) x)
             \. [["west" [(dec x) y]]]
             \- [["west" [(dec x) y]]]
             \| [["north" [x (dec y)]] ["south" [x (inc y)]]]
             \\ [["north" [x (dec y)]]]
             \/ [["south" [x (inc y)]]])
    "north" (case ((matrix y) x)
              \. [["north" [x (dec y)]]]
              \| [["north" [x (dec y)]]]
              \- [["west" [(dec x) y]] ["east" [(inc x) y]]]
              \\ [["west" [(dec x) y]]]
              \/ [["east" [(inc x) y]]])
    "south" (case ((matrix y) x)
              \. [["south" [x (inc y)]]]
              \| [["south" [x (inc y)]]]
              \- [["west" [(dec x) y]] ["east" [(inc x) y]]]
              \\ [["east" [(inc x) y]]]
              \/ [["west" [(dec x) y]]])))
(defn conj' [col [a b :as xs]]
  ; to make the main function a bit nicer
  (if (= (count xs) 1)
    (conj col a)
    (conj (conj col a) b)))

(defn bfs
  [init-move matrix]
  (loop [seen  (transient #{}) collect  (transient #{}) moves [init-move]]
    (if-let [current (peek moves)]
      (let [h (hash current)
            [direction [x y :as position]] current]
        (if (or (not (< -1 x (count (first matrix))))
                (not (< -1 y (count matrix)))
                (seen h))
          (recur seen collect (pop moves))
          (let [moves' (next-move position direction matrix)]
            (recur (conj! seen h) (conj! collect (+ (* (count matrix) y) x))
                   (conj' (pop moves) moves')))))
      (count collect))))

(defn solve
  [input-file]
  (let [matrix (->> (u/read-raw-input input-file)
                    u/to-matrix) l (dec (count matrix))]
    [(->>
       (bfs ["east" [0 0]] matrix))
     ;(->> (vec (concat [] (map (fn [col] ["south" [col 0]]) (range 0 (count (first matrix))))
     ;                  (map (fn [col] ["north" [col (dec (count matrix))]]) (range 0 (count (first matrix))))
     ;                  (map (fn [row] ["east" [0 row]]) (range 0 (count matrix)))
     ;                  (map (fn [row] ["west" [(dec (count (first matrix))) row]]) (range 0 (count matrix)))))
     ;     (pmap (fn [n] (bfs n matrix)))
     ;     (map count)
     ;     (apply max))
     (->> (pmap (juxt #(bfs ["south" [% 0]] matrix)
                      #(bfs ["north" [% l]] matrix)
                      #(bfs ["east" [0 %]] matrix)
                      #(bfs ["west" [l %] ] matrix))
                (range (count matrix)))
          flatten
          (apply max))]))




(comment
  (time (solve 16)))












