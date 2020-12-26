(ns aoc-2020.day22
  (:require [clojure.string :as str]))

(defn- to-bottom [cards winner bottom-cards]
  (let [loser (if (= 1 winner) 2 1)
        win-cards (assoc cards winner (into [] (rest (concat (cards winner) bottom-cards))))
        lose-cards (assoc win-cards loser (into [] (rest (cards loser))))]
    lose-cards))

(defn- round [cards]
  (let [top1 (first (cards 1))
        top2 (first (cards 2))]
    (if (> top1 top2)
      (to-bottom cards 1 [top1 top2])
      (to-bottom cards 2 [top2 top1]))))

(defn- round-rec [cards fnc]
  (let [top1 (first (cards 1))
        top2 (first (cards 2))
        l1 (dec (count (cards 1))) 
        l2 (dec (count (cards 2)))]
    (if (and (<= top1 l1) (<= top2 l2))
      (let [cs1 (into [] (take top1 (rest (cards 1))))
            cs2 (into [] (take top2 (rest (cards 2))))
            [winner _] (fnc (assoc (assoc cards 2 cs2) 1 cs1))]
        (if (= 1 winner)
          (to-bottom cards 1 [top1 top2])
          (to-bottom cards 2 [top2 top1])))
      (round cards))))

(defn- score [cs]
  (apply + (map-indexed (fn [idx item] (* (- (count cs) idx) item)) cs)))

(defn- play2 [cards]
  (println "----> play2 " cards)
  (loop [cs cards
         configs #{}]
    (cond 
      (contains? configs cs) [1 (score (cs 1))]
      (empty? (cs 1)) [2 (score (cs 2))]
      (empty? (cs 2)) [1 (score (cs 1))]
      :otherwise 
      (let [nr (round-rec cs play2)]
        (recur nr (conj configs cs))))))

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

(defn part1 [l]
  (play (parse-lines l)))

(defn part2 [l]
  (play2 (parse-lines l)))

(def lines (str/split-lines (slurp "resources/day22_input.txt")))

(println "Day 22 - 1: " (part1 input)) ; 35202
(println "Day 22 - 2: " (part2 input)) ; 32317
