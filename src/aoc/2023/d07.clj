(ns aoc.2023.d07
  (:require
    [aoc.aoc :as aoc]
    [clojure.core.match :refer [match]]
    [clojure.string :as str]))

(use 'debux.core)

(defn change-cards-representation [hand jokers?]
  (let [replacements {"T" "A"
                      "J" (if jokers? "0" "B")
                      "Q" "C"
                      "K" "D"
                      "A" "E"}]
    (str/replace hand #"T|J|Q|K|A" replacements)))


(defn parse-hand [jokers? [cards bid]]
  (let [cards' (change-cards-representation cards jokers?)
        jokers (aoc/count-if #(= \0 %) cards')
        card-counts (->> (str/replace cards' "0" "")
                         frequencies
                         vals
                         (sort >)
                         vec)
        hand-type (match card-counts
                         []        [jokers 0]
                         [x]       [(+ jokers x) 0]
                         [x y & _] [(+ jokers x) y])]
    [hand-type cards' (parse-long bid)]))


(defn calc-score [hands]
  (transduce
    (map (fn [[rank [_ _ bid]]]
           (* rank bid)))
    +
    (map-indexed vector hands)))


(dbgn(defn total-winnings [hands jokers?]
       (->> hands
            (map (partial parse-hand jokers?))
            sort
            (into [[0 "" 0]])                                   ; to have ranks start at 1
            calc-score)))


(defn solve [input-file]
  (let [hands (aoc/read-input input-file :words)]
    [(total-winnings hands false)
     (total-winnings hands true)]))


(solve 7)


