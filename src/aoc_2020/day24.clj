(ns aoc-2020.day24
  (:require [clojure.string :as str]
            [clojure.set :as set]))

; your code here

(def movement 
  [ 
   [1 0 -1] ; e
   [0 1 -1] ; ne
   [1 -1 0] ; se
   [-1 0 1] ; w
   [-1 1 0] ; nw
   [0 -1 1] ; sw
   ])

(def v-add 
  (memoize
    (fn [v p]
      (map 
        (partial apply +) 
        (map vector v p)))))

(defn- move [line]
  (loop [[l & ls] line
         pos [0 0 0]
         ymod 0]
    (if (nil? l)
      pos
      (cond
        (= l \n) (recur ls pos 1)
        (= l \s) (recur ls pos 2)
        (= l \e) (recur ls (v-add pos (nth movement ymod)) 0)
        (= l \w) (recur ls (v-add pos (nth movement (+ ymod 3))) 0)))))

(defn- start-pattern [lines]
  (filter 
    #(odd? (second %)) 
    (frequencies 
      (map move lines))))

(defn part1 [lines]
  (count (start-pattern lines)))

(defn- black-neighbours [lines pos]
  (for [mv movement
        :let [n (v-add mv pos)]
        :when (contains? lines n)]
    n))

(defn- z-value [x y] (* -1 (+ x y)))

(def min-x (memoize (fn [xs] (apply min xs))))
(def max-x (memoize (fn [xs] (apply max xs))))
(def min-y (memoize (fn [ys] (apply min ys))))
(def max-y (memoize (fn [ys] (apply max ys))))

(defn- round [btiles]
  (let [xs (map first btiles)
        ys (map second btiles)]
    (for [x (range (dec (min-x xs)) (+ (max-x xs) 2))
          y (range (dec (min-y ys)) (+ (max-y ys) 2))
          :let [pos [x y (z-value x y)]
                black? (contains? btiles pos)
                bns (count (black-neighbours btiles pos))]
          :when (or (and black? (<= 1 bns 2)) (and (not black?) (= 2 bns)))]
        pos)))

(defn part2 [lines]
  (loop [btiles (into #{} (map first (start-pattern lines)))
         n 0]
    (if (= n 100)
      (count btiles)
      (recur (into #{} (round btiles)) (inc n)))))

(def input (str/split-lines (slurp "resources/day24_input.txt")))

(println "Day 24 - 1: " (part1 input)) ; 377
(println "Day 24 - 2: " (part2 input)) ; 4231
