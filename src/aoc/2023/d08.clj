(ns aoc.2023.d08
  (:require [aoc.file-util :as f]
            [clojure.string :as str]))

(def input (f/read-lines "2023/d08.txt"))

(defn extract-data [s]
  (let [regex #"\b(\w+)\s*=\s*\(([\w\s,]+)\)"]
    (->> (re-seq regex s)
         (map (fn [[_ key value]] [key (str/split value #"\s*,\s*")]))
         (into {}))))

(defn infinite-sequence [items]
  (lazy-cat items (infinite-sequence items)))

(defn search [func maps l commands]
  (reduce (fn [acc n] (
                        if (func acc n)
                        (reduced (inc (second acc)))
                        [(get maps (nth (first acc) n)) (inc (second acc))])) [l 0] commands))

(defn ends-with-a? [char key]
  (.endsWith (str key) char))

(defn filter-by-key [data]
  (->> data
       (keys)
       (filter (partial ends-with-a? "A"))
       (map #(get data %))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [& numbers]
  (let [product (reduce * numbers)]
    (/ product (apply gcd numbers))))

(defn part-1 [input]
  (let [commands (infinite-sequence (map #(if (= % "R") 1 0) (str/split (str/trim (first input)) #"")))
        maps (extract-data (str/join (drop 2 input)))
        firstMap (get maps "AAA")]
    (search (fn [acc n] (= (nth (first acc) n) "ZZZ")) maps firstMap commands)))

(defn part-2 [input]
  (let [commands (infinite-sequence (map #(if (= % "R") 1 0) (str/split (str/trim (first input)) #"")))
        maps (extract-data (str/join (drop 2 input)))
        firsts (filter-by-key maps)]
    (->>
      (mapv #(search (fn [acc n] (ends-with-a? "Z" (nth (first acc) n))) maps % commands) firsts)
      (reduce lcm))))






(defn deal-game
  [all-maps {:keys [cmap count]} n]
  do
  (println "---2222-" (nth (first cmap) n) count n)
  (if (every? (fn [cur] (ends-with-a? "Z" (nth cur n)))

              cmap)
    (reduced {:cmap (map (fn [r] (do
                                   (get all-maps (nth r n)))) cmap) :count (inc count)})
    {:cmap (map (fn [r] (do
                          (get all-maps (nth r n)))) cmap) :count (inc count)}))


(defn part-2-1 [input]
  (let [command (map #(if (= % "R") 1 0) (str/split (str/trim (first input)) #""))
        maps (extract-data (str/join (drop 2 input)))
        firstMap (filter-by-key maps)]

    (do
      (->> (infinite-sequence command)
           (reduce (partial deal-game maps) {:cmap firstMap :count 0}))))



  (part-1 input)
  (part-2 input))


