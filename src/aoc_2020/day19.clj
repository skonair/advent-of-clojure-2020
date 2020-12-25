(ns aoc-2020.day19
  (:require [clojure.string :as str]))

; your code here
(defn- parse-line [line]
  (let [[n & rs] (str/split (str/replace line #"\"" "") #"[:|]")
        rules (map #(str/split (str/trim (str/join %)) #"\s") rs) ]
    {n rules}))

(defn- specific? [rule]
  (and 
    (not (Character/isDigit (first rule)))))

; 0 = [[1 2 3] [2 3]] or 1 = [[a]] 2 = [[b]] 3 = [[c]]

(defn- all-letters [rules rn]
  (if (specific? rn)
    rn
    (for [one-path (rules rn)]
    (str "("
        (for [one-letter one-path] 
      (apply str/join 
          (all-letters rules one-letter)))
           ")"))))

      
(defn part1 [rules]
  (paths (first (all-letters rules "0")) []))


(defn part2 []
)


(def lines (str/split-lines (slurp "resources/day19_input.txt")))

(println "Day 19 - 1: " (part1 input))
(println "Day 19 - 2: " (part2 input))
