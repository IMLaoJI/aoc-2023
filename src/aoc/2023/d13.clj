(ns aoc.2023.d13
  (:require [aoc.aoc :as aoc]))
(use 'debux.core)
(dbgn(defn differences [a b]
       (aoc/count-if false? (map = a b))))


(dbgn(defn is-mirror? [part pattern nrettap line]
       (let [before (take-last line nrettap)
             after (drop line pattern)
             diffs (map differences before after)]
         (case part
           1 (every? zero? diffs)
           2 (= 1 (reduce + diffs))))))


(dbgn(defn mirror-line [part pattern]
       (aoc/find-first
         (partial is-mirror? part pattern (rseq pattern))
         (range 1 (count pattern)))))


(dbgn(defn find-mirror [part pattern]
       (if-let [horizontal-mirror (mirror-line part #p pattern)]
         (* 100 horizontal-mirror)
         (mirror-line part (aoc/transpose pattern)))))


(dbgn(defn notes-sum [patterns part]
       (reduce + (map (partial find-mirror part) patterns))))


(defn solve [input-file]
  (let [patterns (aoc/read-input-paragraphs input-file)]
    [(notes-sum patterns 1)
     (notes-sum patterns 2)]))


(solve 13)