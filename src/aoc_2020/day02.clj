(ns aoc-2020.day02
  (:require [clojure.string :as str]))

(defn correct-pwd? [min max char text]
  (let [n (count (filter #{char} text))]
    (and (<= n max) (>= n min)))) 

(defn match-pwd-policy? [one two char text]
  (let [c1 (nth text (dec one))
        c2 (nth text (dec two))]
    (and 
      (or (= c1 char) (= c2 char))
      (not= c1 c2))))

(defn- correct-passwords [f lines]
  (count
    (filter 
      (partial apply f)
      lines)))

(defn part1 [lines]
  (correct-passwords correct-pwd? lines))

(defn part2 [lines]
  (correct-passwords match-pwd-policy? lines))

(defn- parse-line [line]
  (let [[min max char _ text] (str/split line #"[ \- :]")]
    [(Integer. min) (Integer. max) (first char) text]))

(def lines (map parse-line (str/split-lines (slurp "resources/day02_input.txt"))))

(println "Day 02 - 1: " (part1 lines)) ; 378
(println "Day 02 - 2: " (part2 lines)) ; 280
