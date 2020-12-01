(ns aoc-2020.day01
  (:require [clojure.string :as str]))

(defn sum2020? [nbs]
  (= 2020 (apply + nbs)))

(defn part1 [lines]
  (first
    (for [a lines
          b lines
          :when (and (> b a) (sum2020? [a b]))]
    [a b (* a b)])))

(defn part2 [lines]
  (first
    (for [a lines
          b lines
          c lines
          :when (and (> c b) (> b a) (sum2020? [a b c]))]
    [a b c (* a b c)])))

