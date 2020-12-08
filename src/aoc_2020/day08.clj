(ns aoc-2020.day08
  (:require [clojure.string :as str]))

(defn- step [lines pc akk]
  (let [[code value] (nth lines pc)]
    (cond
      (= code "nop") [(inc pc) akk]
      (= code "acc") [(inc pc) (+ akk value)]
      (= code "jmp") [(+ pc value) akk])))

(defn run [lines]
  (loop [pc 0
         akk 0
         visited #{}]
    (cond 
      (contains? visited pc) [false akk] ; stop when loop detected
      (>= pc (count lines)) [true akk] ; stop when program terminates
      :otherwise (let [[npc nakk] (step lines pc akk)] ; otherwise run the instruction
                   (recur npc nakk (conj visited pc))))))

(defn- parse-line [line]
  (let [[code param] (str/split line #" ")]
    [code (Integer. param)]))

(defn part1 [lines]
  (run lines))

(defn- replace-jmp-with-nop [lines]
  (for [i (range (count lines))] 
    (map-indexed 
      (fn [idx e] 
        (if 
          (and 
            (= "jmp" (first e)) ; for my input it was sufficient to replace jmp with nop
            (= i idx)) 
          ["nop" (second e)] e)) 
      lines)))

(defn part2 [lines]
  (filter 
    #(first %)
    (distinct 
      (map run (replace-jump-with-nop lines)))))

(def lines 
  (->> (slurp "resources/day08_input.txt")
    str/split-lines
    (map parse-line)))

(println "Day 08 - 1: " (part1 input)) ; 1475
(println "Day 08 - 2: " (part2 input)) ; 1270
