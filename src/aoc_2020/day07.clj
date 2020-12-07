(ns aoc-2020.day07
  (:require [clojure.string :as str]))

(def contains-sg? 
  (memoize 
    (fn [color lines]
      (let [hm (lines color)
            colors (keys hm)]
        (if (hm "shiny gold") 
          true
          (some identity (for [c colors] (contains-sg? c lines))))))))

(defn part1 [lines]
  (count
    ; all colored bags that contain the shiny gold one on any level
    (filter 
      identity 
      (map #(contains-sg? % lines) (keys lines)))))

(def count-bags 
  (memoize 
    (fn [color lines]
      (let [hm (lines color)
            colors (keys hm)]
        (if (empty? colors)
          1
          (apply 
            +
            1 ; this one bag plus the ones in the bag
            (for [c colors]
              (* (Integer. (hm c)) (count-bags c lines)))))))))

(defn part2 [lines]
  (dec ; without the shiny gold one
    (count-bags "shiny gold" lines)))


; lines looks like
; muted green bags contain 2 posh lavender bags, 2 faded indigo bags, 2 mirrored black bags, 5 clear magenta bags.
(defn- parse-line [line]
  (let [color (str/join " " (take 2 (str/split line #" ")))
        m (re-matcher #"(\d+)\s([a-z]+\s[a-z]+)\sbag" line)]
    (hash-map color
      (loop [f (re-find m)
             h {}]
        (if (nil? f)
          h
          (let [[_ n c] f]
            (recur (re-find m) (into h {c (Integer. n)}))))))))

(def input (str/split-lines (slurp "resources/day07_input.txt")))
(def lines (into {} (map parse-line input)))

(println "Day 07 - 1: " (part1 input)) ; 242
(println "Day 07 - 2: " (part2 input)) ; 176035
