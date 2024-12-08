(ns aoc.2023.d19
  (:require [aoc.aoc :as aoc]
            [clojure.string :as str]
            [debux.core :refer :all]))

(add-tap (bound-fn* clojure.pprint/pprint))
(add-tap (bound-fn* prn))
(def debug-a (atom nil))
(add-tap #(reset! debug-a %))

(defn parse-rule [line]
  (let [[cond dest] (str/split line  #":")]
    (if (nil? dest)
      [(keyword cond)]
      (let [[s f & n]  cond
            func ({\> > \< <} f)
            nums (parse-long (apply str n))]
        [(keyword dest) func (keyword  (str s)) nums]))))

(defn parse-workflow [line]
  (let [[_ cname rule]  (re-find #"(.+)\{(.+)\}" line)
        k (keyword cname)
        rules  (map parse-rule (str/split rule #","))]
    {k rules}))

(defn parse-rating
  [line]
  (let [[x m a s]  (aoc/integers line)]
    {:x x :m m :a a :s s}))

(defn find-rating
  [workflows rating]
  (loop [command :in]
    (case command
      :R 0
      :A (reduce + (vals rating))
      (recur (loop [[[ dest func s nums] & other] (workflows command)]
              (cond
                (nil? func) dest
                (func (rating s) nums) dest
                :else
                (recur other)))))))

(defn spread-out
  [rules ratings]
  (loop [[[dest func s nums] & other] rules
         out []
         ratings ratings]
   (cond
     (nil? func) (conj out [dest ratings])
     (= func <) (let [[a b] (ratings s)]
                  (recur other
                         (conj out [dest (assoc ratings s [a (dec nums)])])
                         (assoc ratings s [nums b])))
     (= func >) (let [[a b] (ratings s)]
                  (recur other
                         (conj out [dest (assoc ratings s [(inc nums) b])])
                         (assoc ratings s [a nums]))))))

(defn find-result
  [workflows ratings]
  (loop [stack [[:in ratings]]
         score 0]
        (if-let [[command ratings] (peek stack)]
          (let [stack' (pop stack)]
           (case command
             :R (recur stack' score)
             :A (recur stack' (+ score (reduce * (map (fn [[a b]] (- (inc b) a))
                                                      (vals ratings)))))
            (recur (into stack' (spread-out (workflows command) ratings) ) score)))

          score)))

(defn solve [input-file]
  (let [[wf rts] (aoc/read-input-paragraphs input-file)
        workflows (into {} (map parse-workflow wf))
        ratings (map parse-rating  rts)
        ratings-2  #p(zipmap [:x :m :a :s] (repeat [1 4000]))]
      [(reduce + (map (partial find-rating workflows) ratings))
       (find-result workflows ratings-2)]))





;(time (solve 19))