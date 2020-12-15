(ns aoc-2020.day15
  (:require [clojure.string :as str]))

(defn- two-highest [numbers]
  (max 0 (apply - numbers)))

(defn- add-maxtwo [turn c]
  (if (empty? c) 
    [turn]
    (let [m (apply max c)]
      [turn m])))

(defn- next-number [last-turn last-number numbers]
  (let [new-number (two-highest (numbers last-number))]
    [new-number (assoc numbers new-number (add-maxtwo (inc last-turn) (numbers new-number)))])) 

(defn run [numbers stopnum]
  (loop [nums numbers
         last-turn (apply max (flatten (vals numbers)))
         last-number (first (first (filter #(some #{last-turn} (val %)) numbers)))]
    (if (= stopnum last-turn)
      last-number
      (let [[new-number new-numbers] (next-number last-turn last-number nums)]
        (recur new-numbers (inc last-turn) new-number)))))

(defn part1 [numbers]
  (run numbers 2020))

(defn part2 [numbers]
  (run numbers 30000000))

(def input (slurp "resources/day15_input.txt"))

(println "Day 15 - 1: " (part1 {15 [1] 12 [2] 0 [3] 14 [4] 3 [5] 1 [6]})) ; 249
(println "Day 15 - 2: " (part2 {15 [1] 12 [2] 0 [3] 14 [4] 3 [5] 1 [6]})) ; 41687
