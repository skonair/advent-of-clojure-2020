(ns aoc-2020.day05
  (:require [clojure.string :as str]))

(defn- binify [s]
  (-> s
    (str/replace #"[FL]" "0")
    (str/replace #"[BR]" "1")
    (str/trim)
    (Integer/parseInt 2)))

(defn- seat-id [line]
  (let [row (str/join (take 7 line))
        seat (str/join (drop 7 line))]
    (+ (* 8 (binify row)) (binify seat))))

(defn part1 [lines]
  (apply max (map seat-id lines)))

(defn part2 [lines]
  (let [seats (map seat-id lines)
        mins (apply min seats)
        maxs (apply max seats)]
    (first 
      (for [x (range mins maxs) 
            :when (empty? (filter #(= x %) seats))]
        x))))

(def input (slurp "resources/day05_input.txt"))
(def lines (str/split-lines input))

(println "Day 05 - 1: " (part1 input)) ; 822
(println "Day 05 - 2: " (part2 input)) ; 705
