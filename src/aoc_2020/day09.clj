(ns aoc-2020.day09
  (:require [clojure.string :as str]))

(defn- not-summable? [n numbers]
  (empty? 
    (for [n1 numbers
          n2 numbers
          :when (= n (+ n1 n2))]
      n)))

(defn part1 [pre numbers]
  (first 
    (for [p (partition (inc pre) 1 numbers)
          :when (not-summable? (last p) (drop-last p))]
        (last p))))

(defn- contigous-range [n numbers]
  (first 
    (for [a (range 2 (count numbers))
          b (partition a 1 numbers)
          :when (= n (apply + b))]
      b)))

(defn part2 [n numbers]
  (let [cr (contigous-range n numbers)]
    (+ (apply min cr) (apply max cr))))

(def lines (map #(Long. %) (str/split-lines (slurp "resources/day09_input.txt"))))

(println "Day 09 - 1: " (part1 25 lines)) ; 21806024
(println "Day 09 - 2: " (part2 21806024 lines)) ; 2986195
