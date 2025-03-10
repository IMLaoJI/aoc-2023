(ns aoc.aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(def empty-queue PersistentQueue/EMPTY)

(defn integers
  [s & {:keys [negative?]
        :or   {negative? true}}]
  (mapv parse-long
        (re-seq (if negative? #"-?\d+" #"\d+") s)))

(defn string->digits [s]
  (->> (str/split s #"")
       (map parse-long)
       (filterv some?)))


(defn parse-multiline-string
  [s & [parse-fn {:keys [word-sep nl-sep]}]]
  (->> (str/split s (or nl-sep #"\r\n"))
       ((if (nil? parse-fn) identity
                            (partial mapv
                                     (case parse-fn
                                       :int parse-long
                                       :ints integers
                                       :digits string->digits
                                       :chars vec
                                       :words #(str/split % (or word-sep #" "))
                                       parse-fn))))))



(defn get-puzzle-input [n & flags]
  (let [test? (some #{:test} flags)]
    (-> (str "2023/" (format "d%02d" n) (when test? ".test") ".txt")
        io/resource
        slurp)))


(defn read-input
  [input & [parse-fn seps]]
  (let [name (if (int? input)
               (format "d%02d" input)
               input)]
    (-> (str "2023/" name ".txt")
        io/resource
        slurp
        (parse-multiline-string parse-fn seps))))

(defn read-input-line

  [input & [parse-fn word-sep]]

  (-> input
      (read-input parse-fn {:word-sep word-sep})
      first))

(defn read-input-paragraphs

  [input & [parse-fn word-sep]]
  (->> (read-input input nil {:nl-sep #"\r\n\r\n"})
       (mapv #(parse-multiline-string % parse-fn {:word-sep word-sep}))))

(defn transpose [matrix]
  (apply mapv vector matrix))


(defn manhattan ^long
  ([p] (manhattan p [0 0]))
  ([[^long x1 ^long y1] [^long x2 ^long y2]]
   (+ (abs (- x1 x2))
      (abs (- y1 y2)))))


(defn ord [s]
  (int (first s)))


(defn pt+ ^longs [[^long ax ^long ay] [^long bx ^long by]]
  [(+ ax bx) (+ ay by)])

(defn neighbours ^longs [[^long x ^long y] ^long amount]
  (for [^long dy [-1 0 1]
        ^long dx [-1 0 1]
        :when
        (case amount
          4 (odd? (- dx dy))
          5 (<= (+ (abs dx) (abs dy)) 1)
          8 (not= dx dy 0)
          9 true)]
    [(+ x dx) (+ y dy)]))

(defn neighbours-3d [[^long x ^long y ^long z]]
  [[(dec x) y z] [(inc x) y z]
   [x (dec y) z] [x (inc y) z]
   [x y (dec z)] [x y (inc z)]])


(defn grid->points
  ([v] (grid->points v identity))
  ([v pred]
   (into {}
         (for [[y line] (map-indexed vector v)
               [x char] (map-indexed vector line)
               :when (pred char)]
           [[x y] char]))))


(defn none? [pred xs]
  ;; Faster version of `not-any?`.
  (reduce
    (fn [acc x]
      (if (pred x)
        (reduced false)
        acc))
    true
    xs))

(defn array-none? [pred ^longs arr]
  ;; Much much faster version of `not-any?` for long-arrays.
  (loop [idx (dec (alength arr))
         acc true]
    (if (neg? idx)
      acc
      (if (pred (aget arr idx))
        false
        (recur (dec idx) acc)))))

(defn count-if [pred xs]
  (reduce
    (fn [^long acc x]
      (if (pred x) (inc acc) acc))
    0
    xs))

(defn find-first [pred xs]
  (reduce
    (fn [_ x]
      (when (pred x) (reduced x)))
    nil
    xs))


(defn gcd
  ([] 1)
  ([x] x)
  ([a b] (if (zero? b) a
                       (recur b (mod a b)))))

(defn lcm
  ([] 1)
  ([x] x)
  ([a b] (/ (* a b) (gcd a b))))