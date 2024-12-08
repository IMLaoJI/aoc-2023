(ns aoc.2023.d03
  (:require [aoc.file-util :as f]
            [aoc.aoc :as aoc]))

(def input (f/read-lines "2023/d03.txt"))

(def digits (set (apply str (range 10))))

(defn symbol-coords [grid]
  (set (keys (remove #(digits (val %)) grid))))

(defn adjacent-symbols [pt symbs]
  (keep symbs (aoc/neighbours pt 8)))


(defn gear-ratios [gear-values]
  (keep
    #(when (= (count %) 2)
       (reduce * %))
    gear-values))

(defn find-solution [lines symbols gears]
  (let [h (count lines)
        w (count (first lines))
        init-state {:pt            [0 0]
                    :current       ""
                    :near-symbols? false
                    :numbers       []
                    :current-gears #{}
                    :gear-values   {}}]
    (loop [{:keys        [:current :near-symbols? :numbers :current-gears :gear-values]
            [x y :as pt] :pt
            :as          state} init-state]
      (do
        (cond
          (= y h)
          {:nums        numbers
           :gear-ratios (gear-ratios (vals gear-values))}

          (or (= x w)
              (= \. (get (lines y) x))
              (symbols pt))
          (if (and near-symbols? (seq current))
            (let [number (parse-long current)]
              (recur (assoc init-state
                       :pt (if (= x w) [0 (inc y)] [(inc x) y])
                       :numbers (conj numbers number)
                       :gear-values (reduce (fn [gvals gear]
                                             (update gvals gear conj number))
                                           gear-values
                                           current-gears))))
            (recur (assoc init-state
                     :pt (if (= x w) [0 (inc y)] [(inc x) y])
                     :numbers numbers
                     :gear-values gear-values)))
          :else
          (recur (assoc state
                   :pt [(inc x) y]
                   :current (str current (get (lines y) x))
                   :near-symbols? (or near-symbols? (seq (adjacent-symbols pt symbols)))
                   :current-gears (into current-gears (adjacent-symbols pt gears)))))))))




(defn parse-game [input]
  (let [points (aoc/grid->points input #(not= % \.))
        symbols (symbol-coords points)
        gears (set (filter #(= \* (points %)) symbols))]
    (find-solution input symbols gears)))



(defn part-1 [input]
  (->> (parse-game input)
       (#(reduce + (:nums %)))))



(defn part-2 [input]
  (->> (parse-game input)
       (#(reduce + (:gear-ratios %)))))


(comment
  (def points (aoc/grid->points input))
  (def symbs (symbol-coords points))
  (def gears (set (filter #(= \* (points %)) symbs)))
  ((input 2) 3)
  (symbol-coords points)
  (part-1 input)
  (part-2 input)
  (println digits))













