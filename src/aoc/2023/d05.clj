(ns aoc.2023.d05
  (:require [aoc.file-util :as f]
            [clojure.string :as str]))

(def input (f/read-file "2023/d05.txt"))

(defn group-list [n lst]
  (partition n n nil lst))

(defn deal-map
  [store [target source count]]
  (conj store [target source count]))


(defn parse-games [s]
  (let [parts (str/split input #"\r\n\s*\r\n")
        deal_parts (map (partial map parse-long) (map (partial re-seq #"\d+") parts))
        deal_fn (fn [idx] (reduce deal-map [] (group-list 3 (nth deal_parts idx))))]

    (hash-map :seed-ids (first deal_parts)
              :seed-soil (deal_fn 1)
              :soil-to-fertilizer (deal_fn 2)
              :fertilizer-to-water (deal_fn 3)
              :water-to-light (deal_fn 4)
              :light-to-temperature (deal_fn 5)
              :temperature-to-humidity (deal_fn 6)
              :humidity-to-location (deal_fn 7))))

(defn find-target [seed [target source length]]
  (if (and (<= source seed) (<= seed (+ source length)))
    (reduced (+ target (- seed source)))
    seed))



(defn find-locations [game seed-ids]
  (do
    (->> seed-ids
         (map (fn [seed] (reduce (fn [acc s] (reduce find-target acc s)) seed
                                 [(:seed-soil game)
                                  (:soil-to-fertilizer game)
                                  (:fertilizer-to-water game)
                                  (:water-to-light game)
                                  (:light-to-temperature game) (:temperature-to-humidity game)
                                  (:humidity-to-location game)]))))))



(defn part-1 [input]
  (->> (parse-games input)
       (#(find-locations % (:seed-ids %)))
       (apply min)))



(defn find-source [seed [target source length]]
  (if (and (<= target seed) (<= seed (+ target length)))
    (reduced (+ source (- seed target)))
    seed))



(defn is-in-seed [ranges seed]
  (some (fn [[start length]] (and (<= start seed) (<= seed (+ start length)))) ranges))

(defn reverse-find-seed [games location]
  (do
    [location (->> (reduce (fn [acc s] (find-source acc s)) location (:humidity-to-location games))
                   (#(reduce (fn [acc s] (find-source acc s)) % (:temperature-to-humidity games)))
                   (#(reduce (fn [acc s] (find-source acc s)) % (:light-to-temperature games)))
                   (#(reduce (fn [acc s] (find-source acc s)) % (:water-to-light games)))
                   (#(reduce (fn [acc s] (find-source acc s)) % (:fertilizer-to-water games)))
                   (#(reduce (fn [acc s] (find-source acc s)) % (:soil-to-fertilizer games)))
                   (#(reduce (fn [acc s] (find-source acc s)) % (:seed-soil games))))]))




(defn deal-seeds [seeds]
  (map (fn [[start length]] (
                              take length (range start (-> (+ start length) inc))))
       (group-list 2 seeds)))



(comment
  (println input)
  (parse-games input)
  (def games (parse-games input))
  (def seeds (:seed-ids games))
  (def ranges (group-list 2 seeds))
  (println ranges)
  (is-in-seed ranges 80)
  (->> ((comp deal-seeds :seed-ids) games)
       (map (partial find-locations games)))
  ((comp deal-seeds :seed-ids) games)
  (deal-seeds seeds)
  (time (part-1 input))
  (part-2 input)
  (part--2 input)
  (find-locations (parse-games input)))


(defn my-function []
  (Thread/sleep 1000))


(defn timed [f]
  (let [start-time (System/currentTimeMillis)
        _ (f)
        end-time (System/currentTimeMillis)]
    (println "Execution time: " (- end-time start-time) "ms")))