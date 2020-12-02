#!/bin/bash

if (( $# != 1 )); then
  echo "usage: $0 <day>"
  exit 1
fi

cat >src/aoc_2020/day$1.clj <<EOL
(ns aoc-2020.day$1
  (:require [clojure.string :as str]))

; your code here
(defn part1 []
)

(defn part2 []
)

(def input (slurp "resources/day$1_input.txt"))

(println "Day $1 - 1: " (part1 input))
(println "Day $1 - 2: " (part2 input))
EOL

cat >test/aoc_2020/day$1_test.clj <<EOL
(ns aoc-2020.day$1-test
  (:require [clojure.test :refer :all]
            [aoc-2020.day$1 :refer :all]))

(deftest test1
  (testing "something works correct"
    (is (= 0 0))
    (is (= 0 0))))

EOL

touch resources/day$1_input.txt

