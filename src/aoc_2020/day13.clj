(ns aoc-2020.day13
  (:require [clojure.string :as str]))

; your code here
(defn offset [number bus]
  (let [m (mod number bus)]
    (if (zero? m)
      m
      (- bus m))))

(defn part1 [number buses]
  (apply 
    *
    (first
      (sort-by 
        second
        (map 
          #(vector % (offset number %))
          (map second buses))))))

(defn- tfun [time idx prime primeprod]
    (loop [t time]
      (if (zero? (mod (+ idx t) prime))
        t
        (recur (+ t primeprod)))))

(defn part2 [buses]
  (loop [[b1 & bs] buses
         primeprod 1
         time 0]
    (if (nil? b1)
      time
      (let [[idx prime] b1
            newtime (tfun time idx prime primeprod)]
        (recur bs (* primeprod prime) newtime)))))

(def input (str/split-lines (slurp "resources/day13_input.txt")))
(def number (Integer. (first input)))
(def buses (str/split (second input) #","))
(def inservice (map #(vector (first %) (Integer. (second %))) (filter #(not= (second %) "x") (map-indexed (fn [idx itm] [idx itm]) buses))))

(println "Day 13 - 1: " (part1 number inservice)) ; 3882
(println "Day 13 - 2: " (part2 inservice)) ; 867295486378319
