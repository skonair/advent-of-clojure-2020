(ns aoc-2020.day03
  (:require [clojure.string :as str]))

(defn- element-at [lines x y]
  (nth (nth lines y) x))

(defn- tree-value [lines [x y]]
  (let [e (element-at lines x y)]
    (cond 
      (= e \#) 1
      :otherwise 0)))

(defn- move-part [row width [x y]]
  (let [r (mod row 5)]
    (cond
      (= r 0) [(mod (inc x) width) (inc y)]
      (= r 1) [(mod (+ 3 x) width) (inc y)]
      (= r 2) [(mod (+ 5 x) width) (inc y)]
      (= r 3) [(mod (+ 7 x) width) (inc y)]
      (= r 4) [(mod (inc x) width) (+ y 2)])))
      
(defn- count-trees [lines move-pattern]
  (let [width (count (first lines))]
    (loop [[x y :as coord] [0 0]
           trees 0]
      (if (>= y (count lines))
        trees
        (recur 
          (move-part move-pattern width coord)
          (+ trees (tree-value lines coord)))))))

(defn part1 [lines] 
  (count-trees lines 1))

(defn part2 [lines]
  (apply *
    (for [r (range 0 5)]
      (count-trees lines r))))

(def lines (str/split-lines (slurp "resources/day03_input.txt")))

(println "Day 03 - 1: " (part1 input)) ; 294
(println "Day 03 - 2: " (part2 input)) ; 5774564250
