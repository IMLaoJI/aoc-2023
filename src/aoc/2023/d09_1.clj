(ns aoc.2023.d09-1)

;(defn extrapolate [history]
;  (loop [[hd & tl :as values] history
;         res 0]
;    (if (every? zero? values) res
;                              (recur (map - values tl) (+ res hd)))))
;
;
;(defn prev-sum [transformation histories]
;  (->> histories
;       (pmap (comp extrapolate transformation))
;       (reduce +)))
;
;
;(defn solve [input-file]
;  (let [histories (aoc/read-input input-file :ints)]
;    [(prev-sum reverse histories)
;     (prev-sum identity histories)]))


;(solve 9)