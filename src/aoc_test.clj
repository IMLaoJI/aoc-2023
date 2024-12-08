(ns aoc_test
  (:require aoc.aoc
            [clojure.test :refer [deftest testing is run-tests successful?]]
            [aoc.aoc :as aoc]))


(def int-line "123\n-456\n789")

(defn test-parsing
  ([input result] (test-parsing input nil result))
  ([input f result]
   (is (= (aoc/parse-multiline-string input f) result))))

(deftest parsing
  (testing "digits"
    (is (= (aoc/string->digits "123") [1 2 3]))
    (is (= (aoc/string->digits "ab1cd2e") [1 2]))))


