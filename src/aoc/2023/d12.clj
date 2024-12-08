(ns aoc.2023.d12
  (:require [aoc.aoc :as aoc]
            [clojure.string :as str]))

(defn matched? [prefix, string]
  (= (count (keep-indexed #(when (or (= %2 (get string %1)) (= (get string %1) \?)) %2) prefix))
     (count prefix)))

(def cache (atom {}))

(defn matches [prefix group [string groups]]
  (loop [prefix prefix
         group group
         string string
         groups groups
         result 0]
    (cond
      (> (count prefix) (count string)) result

      (contains? @cache [(count prefix) group])
      (@cache [(count prefix) group])

      (= group (count groups))
      (if (matched? (str prefix (apply str (repeat (- (count string) (count prefix)) \.))) string) 1 0)

      :else
      (loop [i 0
             res 0]
        (if (<= i (- (count string) (nth groups group)))
          (let [p (str prefix (if (> group 0) "." "")
                    (apply str (repeat i \.)) (apply str (repeat (nth groups group) \#)))]
            (if (and (<= (count p) (count string)) (matched? p string))
              (recur (inc i) (+ res (matches p (inc group) [string groups])))
              (recur (inc i) res))))
        (do
          (swap! cache assoc [(count prefix) group] result)
          result)))))


(defn solve [input-file]
  (let [strs (aoc/read-input input-file)
        init (->> (map #(str/split % #" ") strs)
                  (map #(assoc % 1 (map parse-long (str/split (last %) #",")))))]
    [
     (->> (map #(matches "" 0 %) init)
          (reduce +))
     (->> (map (fn [[str groups]] [(str/join "?" (repeat 5 str)) (flatten (repeat 5 groups))]) init)
          (map #(matches "" 0 %))
          (reduce +))]))
(comment
  (time (solve 12)))

