(ns head (:import [java.util PriorityQueue Comparator]
                  [java.util Base64]))


(use 'clojure.test)

(set! *unchecked-math* true)

(defprotocol Heap
  "Protocol for heap."
  (push! [this x])
  (pop! [this]))

(defrecord LongHeap [^longs heap, size]
  Heap
  (push! [this x]
    (loop [i (int @size)]
      (if (< 0 i)
        (let [p (quot (dec i) 2)]
          (if (< x (aget heap p))
            (do (aset heap i (aget heap p))
                (recur p))
            (aset heap i (long x))))
        (aset heap 0 (long x))))
    (swap! size inc)
    this)
  (pop! [this]
    (let [ret (aget heap 0)
          x   (aget heap (swap! size dec))]
      (loop [i 0]
        (let [r (+ 1 (* 2 i))]
          (if (< r @size)
            (let [l (+ 2 (* 2 i))
                  c (if (and (< l @size)
                             (< (aget heap l) (aget heap r)))
                      l
                      r)
                  cval (aget heap c)]
              (if (< cval x)
                (do
                  (aset heap i cval)
                  (recur c))
                (aset heap i x)))
            (aset heap i x))))
      ret)))

(defn long-heap
  [size]
  (->LongHeap (long-array size) (atom 0)))

(deftest test-heap
  (is (= 2 (-> (doto (long-heap 1000000)
                 (push! 5)
                 (push! 3)
                 (push! 7)
                 (push! 2)
                 (push! 9))
               pop!))))

(def heap (-> (doto (long-heap 1000000)
                (push! 5)
                (push! 3)
                (push! 7)
                (push! 2)
                (push! 9))))



(def priority-queue (PriorityQueue. (Comparator/reverseOrder)))

(.offer priority-queue 3)

(.offer priority-queue 4)
(println "Queue elements:" (vec (iterator-seq (.iterator priority-queue))))

;(.offer cache-queue 9999)
;(.offer cache-queue 1)
;(.offer cache-queue 2)
;(.poll cache-queue)