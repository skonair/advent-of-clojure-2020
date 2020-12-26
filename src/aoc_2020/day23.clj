(ns aoc-2020.day23
  (:require [clojure.string :as str]))

; your code here

(defn- not-in? [coll e]
  (nil? (some #{e} coll)))

(defn- pos-of [coll e]
  (first (map first (filter #(= (second %) e) (map-indexed (fn [idx item] [idx item]) coll)))))

(defn- dest [pickup n max]
  (loop [d (dec n)]
    (let [newd (if (zero? d) max d)]
      (if (not-in? pickup newd)
        newd
      (recur (dec newd))))))

(defn- round [cups n maxn]
;  (println "round: " n cups maxn)
  (let [len (count cups)
        pos (mod n len)
        pickup (take 3 (drop (inc pos) (cycle cups)))
        remaining (filter #(not-in? pickup %) cups)
        d (dest pickup (nth cups pos) maxn)
        posd (pos-of remaining d)
        prefix (take (mod (inc posd) len) remaining)
        suffix (drop (mod (inc posd) len) remaining) 
        newlist (concat prefix pickup suffix)
        p1 (nth cups pos)
        p2 (pos-of newlist p1)
        delta (mod (- p2 n) len)
        nl (take len (drop delta (cycle newlist)))
        ]
    nl
    ))
    

(defn part1 [cups]
  (let [maxn (apply max cups)]
    (loop [cs cups
           n 0]
      (if (= n 100)
        cs
        (recur (round cs n maxn) (inc n))))))

(defn part2 [cups]
  (let [maxn (apply max cups)
        miocups (concat cups (range 10 1000001))]
    (loop [cs miocups
           n 0]
      (if (zero? (mod n 1000)) (println n))
      (if (= n 10000000)
        cs
        (recur (round cs n maxn) (inc n))))))

(defn part2 []
)

(def input2 (map #(- (int %) 48) "389125467"))
(def input (map #(- (int %) 48) "284573961"))

(println "Day 23 - 1: " (part1 input))
(println "Day 23 - 2: " (part2 input))
