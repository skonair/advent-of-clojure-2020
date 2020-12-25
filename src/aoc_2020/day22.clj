(ns aoc-2020.day22
  (:require [clojure.string :as str]))

; your code here

(defn- round [cards]
  (let [top1 (first (cards 1))
        top2 (first (cards 2))]
    (if (> top1 top2)
      (assoc (assoc cards 2 (into [] (rest (cards 2)))) 1 (into [] (rest (conj (conj (cards 1) top1) top2))))
      (assoc (assoc cards 1 (into [] (rest (cards 1)))) 2 (into [] (rest (conj (conj (cards 2) top2) top1)))))))

(defn- score [cs]
  (apply + (map-indexed (fn [idx item] (* (- (count cs) idx) item)) cs)))

(defn- play [cards]
  (loop [cs cards]
    (cond 
      (empty? (cs 1)) (score (cs 2))
      (empty? (cs 2)) (score (cs 1))
      :otherwise (recur (round cs)))))

(defn- parse-lines [lines]
  (loop [[l & ls] lines
         player 0
         cards {1 [] 2 []}]
    (if (nil? l)
      cards
      (cond
        (str/starts-with? l "Player") (recur ls (inc player) cards)
        (empty? l) (recur ls player cards)
        :otherwise (recur ls player (assoc cards player (conj (cards player) (Integer. l))))))))

(defn part1 [lines]
  (play (parse-lines lines)))

(defn part2 []
)

(def lines (str/split-lines (slurp "resources/day22_input.txt")))

(println "Day 22 - 1: " (part1 input)) ; 35202
(println "Day 22 - 2: " (part2 input))
