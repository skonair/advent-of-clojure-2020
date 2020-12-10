(ns aoc-2020.day10
  (:require [clojure.string :as str]))

(defn- count-jolt-differences [lines]
  (frequencies 
    (map #(- (second %) (first %)) (partition 2 1 lines))))

(defn part1 [lines]
  (let [f (count-jolt-differences lines)]
    (* (f 1) (f 3))))

(def count-distinct-ways
  (memoize
    (fn [lines pos n]
      (if (= pos (count lines))
        1 ; one successful way
        (let [previous (nth lines (dec pos))
              current (nth lines pos)
              nn (+ n (- current previous))]
          (cond
            (> nn 3) 0 ; gap is too high - stop path
            (= nn 3) (count-distinct-ways lines (inc pos) 0) ; gap is three - do not consider any more numbers
            :otherwise (+ ; gap must be 1 or 2
                     (count-distinct-ways lines (inc pos) 0) ; add the next standard path and
                     (count-distinct-ways lines (inc pos) nn)))))))) ; the next path with the gap counter in mind

(defn part2 [lines]
    (count-distinct-ways lines 1 0))

(def lines 
  (let [lines (map #(Integer. %) (str/split-lines (slurp "resources/day10_input.txt")))
        highest (apply max lines)]
    (sort (conj (conj lines 0) (+ 3 highest)))))

(println "Day 10 - 1: " (part1 input)) ; 2470
(println "Day 10 - 2: " (part2 input)) ; 1973822685184
