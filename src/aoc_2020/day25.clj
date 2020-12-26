(ns aoc-2020.day25
  (:require [clojure.string :as str]))

; your code here

(defn- cloop [n sn]
  (loop [cnt n
         v 1]
    (if (zero? cnt)
      v
      (recur (dec cnt) (mod (* v sn) 20201227)))))

(defn- enc-key [pk]
  (loop [i 0
         v 1]
    (if (= pk v)
      i
      (recur (inc i) (mod (* v 7) 20201227)))))

(defn part1 [pk1 pk2]
  (let [l1 (enc-key pk1)]
    (cloop l1 pk2)))

(defn part2 []
)

(def input (slurp "resources/day25_input.txt"))

(println "Day 25 - 1: " (part1 8184785 5293040)) ; 4126980
(println "Day 25 - 2: " (part2 input))
