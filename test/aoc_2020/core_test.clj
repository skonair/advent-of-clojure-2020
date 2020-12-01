(ns aoc-2020.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2020.day01 :refer :all]))

(def lines (str/split-lines (slurp "resources/day01_input.txt")))
(def input (sort (map #(Integer. %) lines)))

(deftest day01-test
  (testing "day01"
    (is (= 270144 (last (part1 input)))
    (is (= 261342720 (last (part2 input)))))))
