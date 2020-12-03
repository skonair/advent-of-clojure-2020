(ns aoc-2020.day03
  (:require [clojure.string :as str]))

(defn- tree-value [lines [x y]]
  (let [e (nth (nth lines y) x)]
    (cond 
      (= e \#) 1
      :otherwise 0)))

(defn- move [[dx dy] width [x y]]
    [(mod (+ x dx) width) (+ y dy)])
      
(defn- count-trees [lines move-pattern]
  (let [width (count (first lines))]
    (loop [[x y :as coord] [0 0]
           trees 0]
      (if (>= y (count lines))
        trees
        (recur 
          (move move-pattern width coord)
          (+ trees (tree-value lines coord)))))))

(defn part1 [lines] 
  (count-trees lines [3 1]))

(defn part2 [lines]
  (apply *
    (for [move-pattern [[1 1] [3 1] [5 1] [7 1] [1 2]]]
      (count-trees lines move-pattern))))

(def lines (str/split-lines (slurp "resources/day03_input.txt")))

(println "Day 03 - 1: " (part1 input)) ; 294
(println "Day 03 - 2: " (part2 input)) ; 5774564250
